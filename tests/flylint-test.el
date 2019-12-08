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
        (buffer  (with-current-buffer (get-buffer-create "*Flylint Test*")
                   (prog1 (current-buffer)
                     (erase-buffer)
                     (insert "\
#include <iostream>

class FooBar {
    int asdf;
};

int main(int argc, const char * argv[]) {
    FooBaz foobar;
    return 0;
}
")))))
  (it "can get"
    (promise-chain (flylint--promise-get-checker checker)
      (then (lambda (res)
              (flylint--debug 'checker
                "done: %s" (symbol-name res)))
            (lambda (res)
              (flylint--debug 'checker
                "fail: %s" (symbol-name res))))

      (then (lambda (res)
              (let ((check (string= res "done: c/c++-gcc")))
                (flylint--debug 'check
                  "check: %s" check))))))

  (it "can exec"
    (promise-chain (flylint--promise-get-checker checker)
      (then (lambda (res)
              (flylint--promise-exec-command checker buffer)))

      (then (lambda (res)
              (let ((check (equal res '(1 "
<stdin>: In function ‘int main(int, const char**)’:
<stdin>:8:5: error: ‘FooBaz’ was not declared in this scope; did you mean ‘FooBar’?
<stdin>:7:14: warning: unused parameter ‘argc’ [-Wunused-parameter]
<stdin>:7:33: warning: unused parameter ‘argv’ [-Wunused-parameter]
"))))
                (flylint--debug 'check
                  "check: %s" check))))))

  (it "can tokenize"
    (promise-chain (flylint--promise-get-checker checker)
      (then (lambda (res)
              (flylint--promise-exec-command checker buffer)))
      (then (lambda (res)
            (flylint--promise-tokenize-output checker res)))

      (then (lambda (res)
              (let ((check (equal res '("<stdin>:8:5: error: ‘FooBaz’ was not declared in this scope; did you mean ‘FooBar’?"
                                        "<stdin>:7:14: warning: unused parameter ‘argc’ [-Wunused-parameter]"
                                        "<stdin>:7:33: warning: unused parameter ‘argv’ [-Wunused-parameter]"))))
                (flylint--debug 'check
                  "check: %s" check))))))

  (it "can parse"
    (promise-chain (flylint--promise-get-checker checker)
      (then (lambda (res)
              (flylint--promise-exec-command checker buffer)))
      (then (lambda (res)
              (flylint--promise-tokenize-output checker res)))
      (then (lambda (res)
              (flylint--promise-parse-output checker res)))

      (then (lambda (res)
              (let ((check (equal res '((error nil "8" "5" "‘FooBaz’ was not declared in this scope; did you mean ‘FooBar’?" nil)
                                        (warning nil "7" "14" "unused parameter ‘argc’ " "-Wunused-parameter")
                                        (warning nil "7" "33" "unused parameter ‘argv’ " "-Wunused-parameter")))))
                (flylint--debug 'check
                  "check: %s" check)))))))

;; (provide 'flylint-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-test.el ends here
