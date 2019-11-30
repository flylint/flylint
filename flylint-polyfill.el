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

;;


;;; Code:

(require 'warnings)


;;; alist
(defun flylint--alist-keys (alist)
  "Get all keys of ALIST."
  (mapcar 'car alist))

(defun flylint--alist-values (alist)
  "Get all values of ALIST."
  (mapcar 'cdr alist))


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

ARGS accept (SYMBOL &rest FORMAT-ARGS &key buffer break).

\(fn SYMBOL FORMAT &rest FORMAT-ARGS &key buffer level break)"
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
              (goto-char (point-max))
              (insert
               (concat
                (and break "\n")
                (format (cadr (assq level warning-levels))
                        (format warning-type-format symbol))
                "\n" msg "\n")))
            (when scroll
              (goto-char (point-max))
              (set-window-point
               (get-buffer-window (current-buffer)) (point-max)))))))))

(provide 'flylint-polyfill)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-polyfill.el ends here
