;;; flylint-option.el --- options for flylint    -*- lexical-binding: t; -*-

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

(provide 'flylint-option)
;;; flylint-option.el ends here
