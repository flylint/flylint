;;; flylint-tests.el --- Test definitions for flylint  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Flylint Community

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

;; Test definitions for `flylint'.


;;; Code:

(load "cort-test")
(require 'flylint)


;;; Interactive tests

(defconst flylint-tests-src-dir (file-name-directory
                           (or load-file-name (buffer-file-name)))
  "Source directory path of `flylint'.")

(defun flylint-tests-make-error ()
  "Make `flylint-error' structure."
  (interactive)
  (flylint-error-new 10 5 'warning "Test warning" :checker 'sample-lint))

(defun flylint-tests-make-overlay ()
  "Make overlay via `flylint--add-overlay'."
  (interactive)
  (flylint--add-overlay (flylint-tests-make-error)))

(defun flylint-tests-get-sample-linter (name)
  "Get sample linter output located in ./scripts/NAME."
  (shell-command-to-string
   (mapconcat #'shell-quote-argument
              `("emacs" "--batch" "-l"
                ,(expand-file-name (concat "scripts/" name)
                                   flylint-tests-src-dir))
              " ")))

;; (provide 'flylint-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-tests.el ends here
