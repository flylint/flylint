;;; flylint.el --- Asynchronous on-the-fly inspection framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Flylint Community

;; Author: Naoya Yamashita <conao3@gmail.com>
;;         USAMI Kenta <tadsan@zonu.me>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26") (async-await "1.0"))
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
(require 'async-await)
(require 'flylint-struct)
(require 'flylint-face)
(require 'flylint-checker)

(defgroup flylint nil
  "Asynchronous on-the-fly inspection."
  :prefix "flylint-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/flylint/flylint"))


;;; Options

(defvar flylint-command-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of Flylint interactive commands.")

(defcustom flylint-keymap-prefix (kbd "C-c /")
  "Prefix for key bindings of Flylint."
  :group 'flylint
  :type 'key-sequence
  :set (lambda (variable key)
         (when (and (boundp variable) (boundp 'flylint-mode-map))
           (define-key flylint-mode-map (symbol-value variable) nil)
           (define-key flylint-mode-map key flylint-command-map))
         (set-default variable key)))

(defcustom flylint-mode-line-prefix "FlyL"
  "Base mode line lighter for Flylint."
  :group 'flylint
  :type 'string)

(defcustom flylint-enable-modes t
  "Modes `flylint-mode' is turned on by `flylint-global-mode'.

If t, Flylint Mode is turned on for all major modes.
If a list, Flylint Mode is turned on for all `major-mode' symbols.

Only has effect when variable `global-flylint-mode' is non-nil.

NOTE:
In the first place, it is not enabled for buffers that should not be
enabled.  See `(elisp)Major Mode Conventions' and `(elisp)Defining
Minor Modes'"
  :group 'flylint
  :type '(choice (const :tag "all" t)
                 (repeat :tag "mode spesific" symbol)))

(defcustom flylint-disable-modes '(view-mode)
  "Modes `flylint-mode' is not turned on by `flylint-global-mode'.

If nil, Flylint Mode is not turned on for all major modes.
If a list, Flylint Mode is not turned on for all `major-mode' symbols.

Only has effect when variable `global-flylint-mode' is non-nil.

NOTE:
In the first place, it is not enabled for buffers that should not be
enabled.  See `(elisp)Major Mode Conventions' and `(elisp)Defining
Minor Modes'"
  :group 'flylint
  :type '(choice (const :tag "not disable" nil)
                 (repeat :tag "mode spesific" symbol)))

(defcustom flylint-indication-fringe 'left-fringe
  "The indication place for Flylint errors and warnings.

This variable controls how Flylint indicates errors in buffers.
May either be `left-fringe', `right-fringe', or nil.

If set to `left-fringe' or `right-fringe', indicate errors and
warnings via icons in the left and right fringe respectively.

If set to nil, do not indicate errors and warnings, but just
highlight them according to `flylint-highlight-elements'."
  :group 'flylint
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Do not indicate" nil))
  :safe #'symbolp)

(defcustom flylint-highlight-elements 'symbol
  "The highlighting elements for Flylint errors and warnings.

The highlighting mode controls how Flylint highlights errors in
buffers.  The following modes are known:

`column'
     Highlight the error column.  If the error does not have a column,
     highlight the whole line.

`symbol'
     Highlight the symbol at the error column, if there is any,
     otherwise behave like `column'.  This is the default.

`sexp'
     Highlight the expression at the error column, if there is
     any, otherwise behave like `column'.  Note that this mode
     can be *very* slow in some major modes.

`line'
     Highlight the whole line.

'nil
     Do not highlight errors at all.  However, errors will still
     be reported in the mode line and in error message popups,
     and indicated according to `flylint-indication-fringe'."
  :group 'flylint
  :type '(choice (const :tag "Highlight columns only" column)
                 (const :tag "Highlight symbols" symbol)
                 (const :tag "Highlight expressions" sexp)
                 (const :tag "Highlight whole lines" line)
                 (const :tag "Do not highlight errors" nil))
  :safe #'symbolp)


;;; Functions

(defun flylint--add-overlay (err)
  "Add overlay for ERR."
  (if (not (flylint-error-p err))
      (error "`flylint--add-overlay' expects ERR is object of `flylint-error', but not")
    (let* ((fringe-icon
            (lambda (level side)
              (when side
                (unless (memq side '(left-fringe right-fringe))
                  (error "Invalid fringe side: %S" side))
                (propertize "!" 'display
                            (list side
                                  'flylint-fringe-double-arrow-bitmap
                                  (flylint--symbol 'fringe-face level))))))
           (region (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (flylint-error-column err))
                       (bounds-of-thing-at-point
                        (or flylint-highlight-elements 'symbol)))))
           (ov     (when region (make-overlay (car region) (cdr region))))
           (level  (flylint-error-level err)))
      (when ov
        (overlay-put ov 'flylint-overlay t)
        (overlay-put ov 'flylint-error err)
        (overlay-put ov 'category (flylint--symbol 'ov-category level))
        (overlay-put ov 'face (when flylint-highlight-elements
                                (flylint--symbol 'face level)))
        (overlay-put ov 'before-string (funcall fringe-icon level flylint-indication-fringe))
        (overlay-put ov 'help-echo (flylint-error-message err))))))

(defun flylint--avairable-checkers (&optional buf)
  "Get avairable checkers for BUF.
If omit BUF, return avairable checkers for `current-buffer'."
  (with-current-buffer (or buf (current-buffer))
    flylint-checker-alist))


;;; Minor-mode support functions / variables

(defvar-local flylint-enable-checkers nil
  "Syntax checker to use for the current buffer.")

(defun flylint--mode-lighter ()
  "Get a text describing status for use in the mode line."
  (let ((errc 0) (warnc 0))
    (format " %s:%s/%s" flylint-mode-line-prefix errc warnc)))

(defun flylint-mode--maybe ()
  "Enable `flylint-mode' if it is safe to do so.

Flylint-mode is enabled for
- major modes listed in `flylint-enable-modes' (if t, enable all)

But Flylint-mode is not enabled for
- the minibuffer
- `fundamental-mode'
- major modes whose `mode-class' property is `special'
- ephemeral buffers (buffer name starts with space)
- encrypted buffers (buffer has variable `epa-file-encrypt-to')
- remote files (see `file-remote-p')
- major modes listed in `flylint-disable-modes'
  (if nil, not disable all)"
  (when (and (or (eq t flylint-enable-modes)
                 (apply 'derived-mode-p flylint-enable-modes))
             (not (or (minibufferp)
                      (eq major-mode 'fundamental-mode)
                      (eq (get major-mode 'mode-class) 'special)
                      (string-prefix-p " " (buffer-name))
                      (local-variable-p 'epa-file-encrypt-to)
                      (and (buffer-file-name)
                           (file-remote-p (buffer-file-name) 'method))
                      (apply 'derived-mode-p flylint-disable-modes))))
    (flylint-mode)))

(defun flylint--setup ()
  "Setup flylint system."
  (setq-local flylint-enable-checkers (flylint--avairable-checkers)))

(defun flylint--teardown ()
  "Teardown flylint system.")


;;; Main

;;;###autoload
(define-minor-mode flylint-mode
  "Minor mode for asynchronous on-the-fly inspection."
  :keymap flylint-command-map
  :lighter (:eval (flylint--mode-lighter))
  :group 'flylint
  (if flylint-mode
      (flylint--setup)
    (flylint--teardown)))

;;;###autoload
(define-globalized-minor-mode global-flylint-mode flylint-mode
  flylint-mode--maybe
  :require 'flylint
  :group 'flylint)

(provide 'flylint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint.el ends here
