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


;;; alist
(defun flylint--alist-keys (alist)
  "Get all keys of ALIST."
  (mapcar 'car alist))

(defun flylint--alist-values (alist)
  "Get all values of ALIST."
  (mapcar 'cdr alist))

(provide 'flylint-polyfill)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-polyfill.el ends here
