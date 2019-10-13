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

(defgroup flylint-checker nil
  "Asynchronous on-the-fly inspection parser."
  :prefix "flylint-checker"
  :group 'tools)


;;; Options


;;; Functions

(defvar flylint-checker-alist nil
  "Avairable parsers for `flylint-checker'.")

(defun flylint-checker--rx-file-name (form)
  "Translate the `(file-name)' FORM into a regular expression."
  (let ((body (or (cdr form) '((minimal-match
                                (one-or-more not-newline))))))
    (rx-to-string `(group-n 1 ,@body) t)))

(defun flylint-checker--rx-message (form)
  "Translate the `(message)' FORM into a regular expression."
  (let ((body (or (cdr form) '((one-or-more not-newline)))))
    (rx-to-string `(group-n 4 ,@body) t)))

(defun flylint-checker--rx-id (form)
  "Translate the `(id)' FORM into a regular expression."
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

`(id SEXP ...)'
     matches an error ID.  SEXP describes the ID.

NO-GROUP is passed to `rx-to-string'.

See `rx' for a complete list of all built-in `rx' forms."
  (let ((rx-constituents
         (append
          `((line . ,(rx (group-n 2 (one-or-more digit))))
            (column . ,(rx (group-n 3 (one-or-more digit))))
            (file-name flylint-checker--rx-file-name 0 nil)
            (message flylint-checker--rx-message 0 nil)
            (id flylint-checker--rx-id 0 nil))
          rx-constituents nil)))
    (rx-to-string form no-group)))

(cl-defstruct (flylint-checker (:constructor flylint-checker--new))
  "Structure representing parser for each linters.
Slots:

`name'
     Parser name, as symbol.

`docstring'
     Parser description, as string.

`command'
     Linter command and args, as pair such as (COMMAND . (ARGS ARGS ...)).

`standard-input'
     Whether the parser uses standard output, as bool.

`working-directory'
     Working directory for parser, as string.

`error-patterns'
     Error patterns linter output, as list.

`enabled'
     Whether parser enabled, as bool.

`modes'
     What major/minor-mode(s) parser enabled, as list.

`next-chekcers'
     Next checkers, as list."
  name docstring command standard-input working-directory
  error-patterns enabled modes next-checkers)

;;;###autoload
(defmacro flylint-checker-define (name &optional docstring &rest args)
  "Define NAME as flylint-checker.
DOCSTRING is an optional documentation string.
ARGS is a list of KEY VALUE pairs, described `flylint-checker'.

\(fn NAME [DOCSTRING] &key COMMAND STANDARD-INPUT WORKING-DIRECTORY
ERROR-PATTERNS ENABLED MODES NEXT-CHECKERS)"
  (declare (indent defun) (doc-string 2))
  (unless (stringp docstring)
    (setq args (append (list docstring) args))
    (setq docstring ""))
  (setq args (append (list :name name :docstring docstring) args))
  (let* ((keywords (list :name :docstring :command
                         :standard-input :working-directory
                         :error-patterns :enabled :modes
                         :next-checkers))
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
                 do (setq val*
                          (mapcar (lambda (elm)
                                    `(,(car elm) .
                                      ,(flylint-checker--rx-to-string
                                        `(: ,@(cdr elm)) 'no-group)))
                                  val*))
                 append (list key* val*))))
    `(push '(,name . ,(apply 'flylint-checker--new args*))
           flylint-checker-alist)))

(defconst flylint-checker-font-lock-keywords
  '(("(\\(flylint-checker-define\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t)))
  "A font-lock regexp of `flylint-checker-define' for `emacs-lisp-mode'.")

(font-lock-add-keywords 'emacs-lisp-mode flylint-checker-font-lock-keywords)


;;; Main

(flylint-checker-define c/c++-gcc
  "A C/C++ syntax checker using GCC.
Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-Wall" "-Wextra")
  :standard-input t
  :error-patterns
  ((info    . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": note: " (message) line-end))
   (warning . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": warning: " (message (one-or-more (not (any "\n["))))
                          (optional "[" (id (one-or-more not-newline)) "]") line-end))
   (error   . (line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": " (or "fatal error" "error") ": " (message) line-end)))
  :modes (c-mode c++-mode))

(provide 'flylint-checker)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-checker.el ends here
