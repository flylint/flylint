;;; flylint-checker.el --- Asynchronous on-the-fly inspection framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Flylint Community

;; Author: Naoya Yamashita <conao3@gmail.com>
;;         USAMI Kenta <tadsan@zonu.me>
;; License: GPL-3.0
;; Homepage: https://github.com/flylint/flylint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flylint is alternative of Flymake and Flycheck.


;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'tabulated-list)
(require 'flylint-polyfill)
(require 'flylint-struct)
(require 'flylint-option)

(defgroup flylint-checker nil
  "Asynchronous on-the-fly inspection parser."
  :prefix "flylint-checker"
  :group 'tools)


;;; Functions

(defvar flylint-checker-alist nil
  "All parsers for `flylint-checker'.")

(defun flylint--get-checker (checker)
  "Return `flylint-chekcer' structure corresponding to CHECKER."
  (alist-get checker flylint-checker-alist))

(define-derived-mode flylint-list-mode tabulated-list-mode "flylint checkers"
  "Major mode for listing `flylint-list-all-checkers'."
  (setq-local tabulated-list-sort-key '("Checker" . nil))
  (setq-local tabulated-list-format [("Checker"     20 t)
                                     ("Modes"       40 t)
                                     ("Description" 40 t)])
  (let ((formatfn (lambda (elm)
                    (if (stringp elm)
                        elm
                      (prin1-to-string (if (eq elm nil) '--- elm))))))
    (setq-local tabulated-list-entries
                (cl-loop
                 for i from 0
                 for elm in
                 (cl-loop
                  for elm in flylint-checker-alist
                  for sym = (car elm)
                  for obj = (cdr elm)
                  for docstring =
                  (let ((str (flylint-checker-docstring obj)))
                    (when str
                      (substring str 0 (or (string-match-p "\n" str)
                                           (length str)))))
                  collect `(,sym
                            ,(flylint-checker-modes obj)
                            ,docstring))
                 collect `(,i ,(apply 'vector (mapcar formatfn elm))))))
  (tabulated-list-print)
  (tabulated-list-init-header))

;;;###autoload
(defun flylint-list-all-checkers ()
  "List all checkers."
  (interactive)
  (let ((buf (get-buffer-create "*Flylint Checkers*")))
    (with-current-buffer buf
      (flylint-list-mode))
    (display-buffer buf)))

(defun flylint-checker--rx-file-name (form)
  "Translate the `(file-name)' FORM into a regular expression."
  (let ((body (or (cdr form) '((minimal-match
                                (one-or-more not-newline))))))
    (rx-to-string `(group-n 1 ,@body) t)))

(defun flylint-checker--rx-message (form)
  "Translate the `(message)' FORM into a regular expression."
  (let ((body (or (cdr form) '((one-or-more not-newline)))))
    (rx-to-string `(group-n 4 ,@body) t)))

(defun flylint-checker--rx-category (form)
  "Translate the `(category)' FORM into a regular expression."
  (rx-to-string `(group-n 5 ,@(cdr form)) t))

(defun flylint-checker--rx-to-string (form &optional no-group)
  "Like `rx-to-string' for FORM, but with special keywords:

`line'
     matches the line number.

`column'
     matches the column number.

`(file-name SEXP ...)'
     matches the file name.  SEXP describes the file name.  If no
     SEXP is given, use a default body of `(minimal-match
     (one-or-more not-newline))'.

`(message SEXP ...)'
     matches the message.  SEXP constitutes the body of the
     message.  If no SEXP is given, use a default body
     of `(one-or-more not-newline)'.

`(category SEXP ...)'
     matches an error CATEGORY.  SEXP describes the CATEGORY.

NO-GROUP is passed to `rx-to-string'.

See `rx' for a complete list of all built-in `rx' forms."
  (let ((rx-constituents
         (append
          `((file-name flylint-checker--rx-file-name 0 nil)
            (line . ,(rx-to-string '(group-n 2 (one-or-more digit)) t))
            (column . ,(rx-to-string '(group-n 3 (one-or-more digit)) t))
            (message flylint-checker--rx-message 0 nil)
            (category flylint-checker--rx-category 0 nil))
          rx-constituents nil)))
    (rx-to-string form no-group)))

;;;###autoload
(defmacro flylint-checker-define (name &optional docstring &rest args)
  "Define NAME as flylint-checker.
DOCSTRING is an optional documentation string.
ARGS is a list of KEY VALUE pairs, described `flylint-checker'.

\(fn NAME [DOCSTRING] &key COMMAND STANDARD-INPUT WORKING-DIRECTORY
ERROR-PATTERNS ERROR-FILTER ENABLED MODES)"
  (declare (indent defun) (doc-string 2))
  (unless (stringp docstring)
    (setq args (append (list docstring) args))
    (setq docstring ""))
  (setq args (append (list :name name :docstring docstring) args))
  (let* ((keywords (list :name :docstring :command
                         :standard-input :working-directory
                         :error-patterns :composed-error-pattern
                         :error-filter :enabled :modes))
         (fn (lambda (elm)
               (if (and (listp elm)
                        (member `',(car elm)
                                `('quote ',backquote-backquote-symbol)))
                   (eval elm)
                 elm)))
         (args* (cl-loop
                 for (key val) on args by 'cddr
                 for key* = (funcall fn key)
                 for val* = (funcall fn val)
                 if (not (keywordp key*)) do (error "%s is not keyword" key*)
                 if (not (memq key* keywords)) do (error "Unrecognize keyword: %s" key*)
                 if (eq key* :error-patterns)
                 do (setq val* `(mapcar
                                 (lambda (elm)
                                   `(,(car elm) .
                                     ,(flylint-checker--rx-to-string
                                       `(: ,@(cdr elm)) 'no-group)))
                                 ',val*))
                 else do (setq val* `',val*)
                 append `(,key* ,val*))))
    (setq args* (append args* `(:composed-error-pattern
                                (rx-to-string
                                 `(| ,@(mapcar (lambda (elm) (list 'regexp (cdr elm)))
                                               ,(plist-get args* :error-patterns)))
                                 'no-group))))
    `(setf (alist-get ',name flylint-checker-alist)
           (flylint-checker--new ,@args*))))


;;; Main

(flylint-checker-define c/c++-gcc
  "A C/C++ syntax checker using GCC.
Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-Wall" "-Wextra"
            "-x" "c++"
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" "/dev/null"
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info    . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": note: " (message) line-end))
   (warning . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": warning: " (message (one-or-more (not (any "\n["))))
                          (optional "[" (category (one-or-more not-newline)) "]") line-end))
   (error   . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": " (or "fatal error" "error") ": " (message) line-end)))
  :modes (c-mode c++-mode))

(defconst flylint-checker-emacs-lisp--check-form
  '(progn
     (defun read-line () (read-string ""))
     (defun get-stdin-buffer ()
       (with-current-buffer (get-buffer-create "*stdin*")
         (prog1 (current-buffer)
           (ignore-errors
             (erase-buffer)
             (let (line)
               (while (setq line (read-line))
                 (insert line "\n")))))))

     (with-current-buffer (get-stdin-buffer)
       (require 'bytecomp)
       (let ((byte-compile-current-file (buffer-name)))
         (byte-compile-from-buffer (current-buffer))))))

(flylint-checker-define emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

See Info Node `(elisp)Byte Compilation'."
  :command ("emacs"
            "-Q" "--batch"
            "--eval" (eval (prin1-to-string
                            flylint-checker-emacs-lisp--check-form)))
  :standard-input t
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":Error:"
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":Warning:"
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end))
  :error-filter
  (lambda (errors)
    (flylint-fill-empty-line-numbers
     (flylint-collapse-error-message-whitespace
      (flylint-sanitize-errors errors))))
  :modes (emacs-lisp-mode lisp-interaction-mode))

(provide 'flylint-checker)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-checker.el ends here
