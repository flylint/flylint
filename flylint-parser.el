;;; flylint-parser.el --- Asynchronous on-the-fly inspection framework  -*- lexical-binding: t; -*-

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

(defgroup flylint-parser nil
  "Asynchronous on-the-fly inspection parser."
  :prefix "flylint-parser"
  :group 'tools)


;;; Options


;;; Functions

(defun flylint-parser--rx-file-name (form)
  "Translate the `(file-name)' FORM into a regular expression."
  (let ((body (or (cdr form) '((minimal-match
                                (one-or-more not-newline))))))
    (rx-to-string `(group-n 1 ,@body) t)))

(defun flylint-parser--rx-message (form)
  "Translate the `(message)' FORM into a regular expression."
  (let ((body (or (cdr form) '((one-or-more not-newline)))))
    (rx-to-string `(group-n 4 ,@body) t)))

(defun flylint-parser--rx-id (form)
  "Translate the `(id)' FORM into a regular expression."
  (rx-to-string `(group-n 5 ,@(cdr form)) t))

(defun flylint-parser--rx-to-string (form &optional no-group)
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
            (file-name flylint-parser--rx-file-name 0 nil)
            (message flylint-parser--rx-message 0 nil)
            (id flylint-parser--rx-id 0 nil))
          rx-constituents nil)))
    (rx-to-string form no-group)))

;;;###autoload
(cl-defmacro flylint-parser-define (name &optional docstring
                                         &key
                                         command
                                         standard-input
                                         working-directory
                                         error-patterns
                                         enabled
                                         modes
                                         next-checkers)
  "Define NAME as flylint-parser.
DOCSTRING is an optional documentation string."
  (declare (indent defun) (doc-string 2))
  (let ((updatefn (lambda (elm)
              (if (and (listp elm)
                       (member `',(car elm)
                               `('quote ',backquote-backquote-symbol)))
                  (eval elm)
                elm))))
    (setq command (funcall updatefn command))
    (setq error-patterns (funcall updatefn error-patterns))
    (setq standard-input (funcall updatefn standard-input))
    (setq working-directory (funcall updatefn working-directory))
    (setq enabled (funcall updatefn enabled))
    (setq modes (funcall updatefn modes))
    (setq next-checkers (funcall updatefn next-checkers)))
  `(list ',name ',docstring ',command ',error-patterns ',standard-input
         ',working-directory ',enabled ',modes ',next-checkers))

(defconst flylint-parser-font-lock-keywords
  '(("(\\(flylint-parser-define\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode flylint-parser-font-lock-keywords)


;;; Main

(flylint-parser-define c/c++-gcc
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

(provide 'flylint-parser)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-parser.el ends here
