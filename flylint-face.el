;;; flylint-face.el --- faces for flylint        -*- lexical-binding: t; -*-

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

(defgroup flylint-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flylint-"
  :group 'flylint)

(defun flylint--symbol (kind target)
  "Return flylint symbol for KIND, TARGET."
  (pcase kind
    (`ov-category     (intern (format "flylint-%s-overlay" target)))
    (`face            (intern (format "flylint-%s-face" target)))
    (`fringe-face     (intern (format "flylint-fringe-%s-face" target)))
    (`error-list-face (intern (format "flylint-error-list-%s-face" target)))
    (_ (error "Call `flylint--symbol' with unkown keyword, %s" kind))))


;;; Faces

(defface flylint-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red"))
    (t
     :underline t :inherit error))
  "Flylint face for errors."
  :group 'flylint-faces)

(defface flylint-warning-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :underline t :inherit warning))
  "Flylint face for warnings."
  :group 'flylint-faces)

(defface flylint-info-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "ForestGreen"))
    (t
     :underline t :inherit success))
  "Flylint face for informational messages."
  :group 'flylint-faces)

(defface flylint-fringe-error-face
  '((t :inherit error))
  "Flylint face for fringe error indicators."
  :group 'flylint-faces)

(defface flylint-fringe-warning-face
  '((t :inherit warning))
  "Flylint face for fringe warning indicators."
  :group 'flylint-faces)

(defface flylint-fringe-info-face
  '((t :inherit success))
  "Flylint face for fringe info indicators."
  :group 'flylint-faces)

(defface flylint-error-list-error-face
  '((t :inherit error))
  "Flylint face for error messages in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-warning-face
  '((t :inherit warning))
  "Flylint face for warning messages in the error list."
  :group 'flylint-faces)

(defface flylint-error-list-info-face
  '((t :inherit success))
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

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'flylint-fringe-double-arrow-bitmap
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b10011000
            #b01101100
            #b00110110
            #b00011011
            #b00110110
            #b01101100
            #b10011000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000)))


(provide 'flylint-face)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-face.el ends here
