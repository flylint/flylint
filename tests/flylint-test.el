;;; flylint-test.el --- Test definitions for flylint  -*- lexical-binding: t; -*-

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

;; Test definitions for `flylint'.


;;; Code:

(require 'buttercup)
(require 'flylint)

(defconst flylint-test-dir (file-name-directory
                            (cond
                             (load-in-progress load-file-name)
                             ((and (boundp 'byte-compile-current-file)
                                   byte-compile-current-file)
                              byte-compile-current-file)
                             (:else (buffer-file-name)))))

(xdescribe "A checker"
  :var ((checker 'c/c++-gcc)
        (buffer (find-file-noselect
                 (expand-file-name "error-sample.el" flylint-test-dir))))
  (it "can get"
    (promise-chain (flylint--promise-get-checker checker)
      (then (lambda (res)
              (flylint--debug 'checker
                "done: %s" (flylint-checker-name (car res))) res)
            (lambda (res)
              (flylint--debug 'checker
                "fail: %s" (flylint-checker-name (car res)))) res)
      (then (lambda (res)
              (let ((check (string= "done: c/c++-gcc" res)))
                (flylint--debug 'check
                  "check: %s" check)))))))

;; (provide 'flylint-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-test.el ends here
