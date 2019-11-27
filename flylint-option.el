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

(defcustom flylint-global-disable-checkers nil
  "Syntax checkers excluded from automatic selection.

A list of Flylint syntax checkers to exclude from automatic
selection.  Flylint will never automatically select a syntax
checker in this list, regardless of the value of
`flylint-checkers'.

However, syntax checkers in this list are still available for
manual selection with `flylint-select-checker'.

Use this variable to disable syntax checkers, instead of removing
the syntax checkers from `flylint-checkers'.  You may also use
this option as a file or directory local variable to disable
specific checkers in individual files and directories
respectively."
  :group 'flylint
  :type '(repeat (symbol :tag "Checker")))

(defcustom flylint-triger-change-delay 0.5
  "How many seconds to wait after a change before checking syntax.

After the buffer was changed, Flylint will wait as many seconds
as the value of this variable before starting a syntax check.  If
the buffer is modified during this time, Flylint will wait
again.

This variable has no effect, if `change' is not contained in
`flylint-check-syntax-triger'."
  :group 'flylint
  :type 'number)

(defcustom flylint-check-syntax-triger (list 'save 'change
                                             'new-line 'mode-enabled)
  "When Flylint should check syntax automatically.

This variable is a list of events that may trigger syntax checks.
The following events are known:

`save'
     Check syntax immediately after the buffer was saved.

`change'
     Check syntax a short time (see `flylint-triger-change-delay')
     after the last change to the buffer.

`new-line'
     Check syntax immediately after a new line was inserted into
     the buffer.

`mode-enabled'
     Check syntax immediately when variable `flylint-mode' is
     non-nil.

Flylint performs a syntax checks only on events, which are
contained in this list.  For instance, if the value of this
variable is `(mode-enabled save)', Flylint will only check if
the mode is enabled or the buffer was saved, but never after
changes to the buffer contents.

If nil, never check syntax automatically.  In this case, use
`flylint-buffer' to start a syntax check manually."
  :group 'flylint
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After the buffer was changed and idle" change)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flylint-mode' was enabled" mode-enabled)))

(defcustom flylint-temp-prefix "flylint"
  "Prefix for temporary files created by Flylint."
  :group 'flylint
  :type 'string)

(defcustom flylint-locate-config-file-functions nil
  "Functions to locate syntax checker configuration files.

Each function in this hook must accept two arguments: The value
of the configuration file variable, and the syntax checker
symbol.  It must return either a string with an absolute path to
the configuration file, or nil, if it cannot locate the
configuration file.

The functions in this hook are called in order of appearance, until a
function returns non-nil.  The configuration file returned by that
function is then given to the syntax checker if it exists.

This variable is an abnormal hook.  See Info node `(elisp)Hooks'."
  :group 'flylint
  :type 'hook)

(provide 'flylint-option)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-option.el ends here
