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
                             (line column level message
                                   &key checker category group corrections
                                   (filename (buffer-file-name))
                                   (buffer (current-buffer))))
               (:copier nil))
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
                             (beg end replace &key (buffer (current-buffer))))
               (:copier nil))
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



(cl-defstruct (flylint-checker (:constructor flylint-checker--new)
                               (:copier nil))
  "Structure representing parser for each linters.
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

`enabled'
     Whether parser enabled, as bool.

`modes'
     What major/minor-mode(s) parser enabled, as list."
  name docstring command standard-input working-directory
  error-patterns enabled modes)

(provide 'flylint-struct)
;;; flylint-struct.el ends here
