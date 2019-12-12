;;; flylint-polyfill.el --- polifill for flylint -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp

;; Copyright (C) 2019  Naoya Yamashita

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flylint is alternative of Flymake and Flycheck.


;;; Code:

(require 'warnings)


;;; alist
(defun flylint--alist-keys (alist)
  "Get all keys of ALIST."
  (mapcar 'car alist))

(defun flylint--alist-values (alist)
  "Get all values of ALIST."
  (mapcar 'cdr alist))


;;; p
(defcustom flylint-p-escape-newlines t
  "Value of `print-escape-newlines' used by p-* functions."
  :type 'boolean
  :group 'p)

(defmacro with-p--working-buffer (form &rest body)
  "Insert FORM, execute BODY, return `buffer-string'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode-variables nil)
     (set-syntax-table emacs-lisp-mode-syntax-table)
     (let ((print-escape-newlines flylint-p-escape-newlines)
           (print-quoted t))
       (prin1 ,form (current-buffer))
       (goto-char (point-min)))
     (progn ,@body)
     (delete-trailing-whitespace)
     (buffer-substring-no-properties (point-min) (point-max))))

(defmacro flylint-p-plist-to-string (form)
  "Output the pretty-printed representation of FORM suitable for plist.
See `flylint-p-plist' to get more info."
  `(with-output-to-string
     (flylint-p-plist ,form)))

(defun flylint-p-plist (form)
  "Output the pretty-printed representation of FORM suitable for plist."
  (progn
    (let ((str (with-p--working-buffer form
                 (forward-char)
                 (ignore-errors
                   (while t (forward-sexp 2) (insert "\n")))
                 (delete-char -1))))
      (princ (concat str "\n")))
    nil))


;;; error
(defcustom flylint-debug-buffer "*Flylint Debug*"
  "Buffer name for flylint debugging."
  :group 'flylint
  :type 'string)

(defcustom flylint-minimum-warning-level :debug
  "Minimum level for `flylint--debug'.
It should be either :debug, :warning, :error, or :emergency"
  :group 'flylint
  :type 'symbol)

(defun flylint--warn (message &rest args)
  "Warn with `flylint' type.
Display a warning message made from (format-message MESSAGE ARGS...)."
  (apply #'lwarn `(flylint :warning ,message ,@args)))

(defun flylint--debug (&rest args)
  "Output debug message to `flylint-debug-buffer'.

FORMAT and FORMAT-ARGS passed `format'.
If BUFFER is specified, output that buffer.
If LEVEL is specified, output higher than `flylint-minimum-warning-level'.
If POPUP is non-nil, `display-buffer' debug buffer.
If BREAK is non-nil, output page break before output string.

ARGS accept (SYMBOL &key buffer level break &rest FORMAT-ARGS).

\(fn &key buffer level break SYMBOL FORMAT &rest FORMAT-ARGS)"
  (declare (indent defun))
  (let ((buffer flylint-debug-buffer)
        (level :debug)
        (popup nil)
        (break nil)
        (symbol 'unknown)
        format format-args elm)
    (while (keywordp (setq elm (pop args)))
      (cond ((eq :buffer elm)
             (setq buffer (pop args)))
            ((eq :popup elm)
             (setq popup (pop args)))
            ((eq :level elm)
             (setq level (pop args)))
            ((eq :break elm)
             (setq break (pop args)))
            (t
             (error "Unknown keyword: %s" elm))))
    (setq symbol elm)
    (setq format (pop args))
    (setq format-args args)
    (with-current-buffer (get-buffer-create buffer)
      (emacs-lisp-mode)
      (when popup
        (display-buffer (current-buffer)))
      (when (<= (warning-numeric-level flylint-minimum-warning-level)
                (warning-numeric-level level))
        (let ((msg (apply #'format `(,format ,@format-args)))
              (scroll (equal (point) (point-max))))
          (prog1 msg
            (save-excursion
              (let ((start (point-max)))
                (goto-char (point-max))
                (insert
                 (concat
                  (and break "\n")
                  (format (cadr (assq level warning-levels))
                          (format warning-type-format symbol))
                  "\n" msg))
                (unless (and (bolp) (eolp)) (newline))))
            (when scroll
              (goto-char (point-max))
              (set-window-point
               (get-buffer-window (current-buffer)) (point-max)))))))))

(provide 'flylint-polyfill)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-polyfill.el ends here
