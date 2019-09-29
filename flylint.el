;;; flylint.el --- Asynchronous on-the-fly inspection framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Flylint Community

;; Author: Naoya Yamashita <conao3@gmail.com>
;;         USAMI Kenta <tadsan@zonu.me>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26") (seml-mode "1.5"))
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

(defgroup flylint nil
  "Asynchronous on-the-fly inspection."
  :prefix "flylint-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/flylint/flylint"))

(defgroup flylint-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flylint-"
  :group 'flylint)


;;; Faces

(defface flylint-error-face '((((supports :underline (:style wave)))
                                :underline (:style wave :color "Red"))
                               (t
                                :underline t :inherit error))
  "Flylint face for errors."
  :group 'flylint-faces)

(defface flylint-warning-face '((((supports :underline (:style wave)))
                                  :underline (:style wave :color "DarkOrange"))
                                 (t
                                  :underline t :inherit warning))
  "Flylint face for warnings."
  :group 'flylint-faces)

(defface flylint-info-face '((((supports :underline (:style wave)))
                               :underline (:style wave :color "ForestGreen"))
                              (t
                               :underline t :inherit success))
  "Flylint face for informational messages."
  :group 'flylint-faces)

(defface flylint-fringe-error-face '((t :inherit error))
  "Flylint face for fringe error indicators."
  :group 'flylint-faces)

(defface flylint-fringe-warning-face '((t :inherit warning))
  "Flylint face for fringe warning indicators."
  :group 'flylint-faces)

(defface flylint-fringe-info-face '((t :inherit success))
  "Flylint face for fringe info indicators."
  :group 'flylint-faces)

(defface flylint-error-list-error-face '((t :inherit error))
  "Flylint face for error messages in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-warning-face '((t :inherit warning))
  "Flylint face for warning messages in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-info-face '((t :inherit success))
  "Flylint face for info messages in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-line-number-face
  '((t :inherit font-lock-constant-face))
  "Face for line numbers in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-column-number-face
  '((t :inherit font-lock-constant-face))
  "Face for line numbers in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-filename-face
  '((t :inherit font-lock-variable-name-face))
  "Face for filenames in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-id-face
  '((t :inherit font-lock-type-face))
  "Face for the error ID in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-id-with-explainer-face
  '((t :inherit flylint-error-list-id
       :box (:style released-button)))
  "Face for the error ID in the error list, for errors that have an explainer."
  :group 'flylint-faces)

(defface flylint-error-list-checker-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for the syntax checker name in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-highlight-face
  '((t :inherit highlight))
  "Flylint face to highlight errors in the error list."
  :group 'flylint-faces)


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


;;; Objects

(cl-defstruct (flylint-error
               (:constructor nil)
               (:constructor flylint-error-new
                             (line column level message
                                   &key checker category group corrections
                                   (filename (buffer-file-name))
                                   (buffer (current-buffer)))))
  "Structure representing an error reported by a syntax checker.
Slots:

`line'
     The line number the error refers to, as number.

`column'
     The column number the error refers to, as number.

`level'
     The error level, as either `info', `warning' or `error'.

`message'
     The error message as a string, if any.

`checker'
     The syntax checker which reported this error, as symbol.

`filename'
     The file name the error refers to, as string.

`buffer'
     The buffer that the error was reported for, as buffer object.

`category' (optional)
     An ID identifying the kind of error.

`group' (optional)
     A symbol identifying the group the error belongs to.

     Some tools will emit multiple errors that relate to the same
     issue (e.g., lifetime errors in Rust).  All related errors
     collected by a checker should have the same `group' value,
     in order to be able to present them to the user.

`corrections' (optional)
     Recommend corrections (list of `flylint-correction')."
  line column level message checker category group filename buffer corrections)

(cl-defstruct (flylint-correction
               (:constructor nil)
               (:constructor flylint-correction-new
                             (beg end replace &key (buffer (current-buffer)))))
  "Structure representing a recommended correction.
Slots:
`beg'
     The beggining of replacement as point object.

`end'
     The end of replacement as point object.

`replace'
     Replace string as string.

`buffer'
     Target buffer as buffer object."
  beg end replace buffer)


;;; Functions

(defun flylint--mode-line-status-text ()
  "Get a text describing status for use in the mode line."
  (let ((errc 0) (warnc 0))
    (format " %s:%s/%s" flylint-mode-line-prefix errc warnc)))


;;; Main

;;;###autoload
(define-minor-mode flylint-mode
  "Minor mode for asynchronous on-the-fly inspection."
  :keymap flylint-command-map
  :lighter (:eval (flylint--mode-line-status-text)))

(provide 'flylint)
;;; flylint.el ends here
