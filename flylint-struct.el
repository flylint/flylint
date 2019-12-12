;;; flylint-struct.el --- struct for flylint     -*- lexical-binding: t; -*-

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

(cl-defstruct (flylint-error
               (:constructor nil)
               (:constructor flylint-error-new
                             (level line column message
                                   &key checker category group corrections
                                   (filename (buffer-file-name))
                                   (buffer (current-buffer))))
               (:copier nil))
  "Structure representing an error reported by a syntax checker.
Slots:

`level'
     The error level, as either `info', `warning' or `error'.

`line'
     The line number the error refers to, as number.

`column'
     The column number the error refers to, as number.

`message'
     The error message as a string, if any.

`filename' (optional)
     The file name the error refers to, as string.

`buffer' (optional)
     The buffer that the error was reported for, as buffer object.

`checker' (optional)
     The syntax checker which reported this error, as symbol.

`category' (optional)
     An category identifying the kind of error.

`group' (optional)
     A symbol identifying the group the error belongs to.

     Some tools will emit multiple errors that relate to the same
     issue (e.g., lifetime errors in Rust).  All related errors
     collected by a checker should have the same `group' value,
     in order to be able to present them to the user.

`corrections' (optional)
     Recommend corrections (list of `flylint-correction')."
  level line column message
  checker category group corrections filename buffer)



(cl-defstruct (flylint-correction
               (:constructor nil)
               (:constructor flylint-correction-new
                             (beg end replace
                                  &key
                                  (filename (buffer-file-name))
                                  (buffer (current-buffer))))
               (:copier nil))
  "Structure representing a recommended correction.

Slots:
`beg'
     The beggining of replacement as point object.

`end'
     The end of replacement as point object.

`replace'
     Replace string as string.

`filename' (optional)
     The file name the error refers to, as string.

`buffer' (optional)
     Target buffer as buffer object."
  beg end replace filename buffer)



(cl-defstruct (flylint-checker (:constructor flylint-checker--new)
                               (:copier nil))
  "Structure representing parser for each linters.

Must create this struct with `flylint-checker-define'.

Slots:
`name'
     Parser name, as symbol.

`docstring'
     Parser description, as string.

`command'
     Linter command and args, as pair such as (COMMAND . (ARGS ARGS ...)).

`standard-input'
     Whether the parser uses standard output, as bool.

`working-directory'
     Working directory for parser, as string.

`error-patterns'
     Error patterns linter output, as list.

`composed-error-pattern'
     Composed error pattern regexp from `error-patterns', as string.

`error-filter'
     Filter the errors returned by this checker, as function.

`enabled'
     Whether parser enabled, as bool.

`modes'
     What major/minor-mode(s) parser enabled, as list."
  name docstring command standard-input working-directory
  error-patterns composed-error-pattern error-filter enabled modes)

(provide 'flylint-struct)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-struct.el ends here
