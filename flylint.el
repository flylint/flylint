;;; flylint.el --- Asynchronous on-the-fly inspection framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Flylint Community

;; Author: Naoya Yamashita <conao3@gmail.com>
;;         USAMI Kenta <tadsan@zonu.me>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26") (async-await "1.0"))
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

(require 'cl-lib)
(require 'async-await)
(require 'flylint-polyfill)
(require 'flylint-struct)
(require 'flylint-option)
(require 'flylint-face)
(require 'flylint-checker)

(defgroup flylint nil
  "Asynchronous on-the-fly inspection."
  :prefix "flylint-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/flylint/flylint"))


;;; Functions

(defvar flylint-mode)
(defvar flylint-enabled-checkers)
(defvar flylint-disabled-checkers)
(defvar flylint-auto-disabled-checkers)
(defvar flylint-temporaries)
(defvar flylint-running)


;;; Manage temp files/directories

(defun flylint--remove-temporary-files ()
  "Remove all temp files and directories created by flylint.
File to remove is listed in `flylint-temporaries' and set to nil."
  (dolist (file-or-dir flylint-temporaries)
    (ignore-errors
      (if (file-directory-p file-or-dir)
          (delete-directory file-or-dir 'recursive)
        (delete-file file-or-dir))))
  (setq flylint-temporaries nil))

(defun flylint--temp-save-buffer (temp-file-fn)
  "Save buffer to temp file returned by TEMP-FILE-FN.
Return the name of the temporary file."
  (let ((filename (funcall temp-file-fn (buffer-file-name))))
    ;; Do not flush short-lived temporary files onto disk
    (let ((write-region-inhibit-fsync t))
      (flylint--save-buffer-to-file filename))
    filename))

(defun flylint--temp-file-system (filename)
  "Create a temporary file named after FILENAME.
If FILENAME is non-nil, this function creates a temporary directory
with `flylint--temp-unique-dir', and creates a file with the same name
as FILENAME in this directory.

Otherwise this function creates a temporary file with
`flylint-temp-prefix' and a random suffix.
Return file will added to `flylint-temporaries'."
  (let ((tempfile (convert-standard-filename
                   (if filename
                       (expand-file-name (file-name-nondirectory filename)
                                         (flylint--temp-unique-dir))
                     (make-temp-file flylint-temp-prefix)))))
    (push tempfile flylint-temporaries)
    tempfile))

(defun flylint--temp-file-inplace (filename)
  "Create an in-place copy of FILENAME prefixed with `flylint-temp-prefix'.
If FILENAME is nil, fall back to `flylint--temp-file-system'.
Return file will added to `flylint-temporaries'."
  (if filename
      (let* ((tempname (format "%s_%s"
                               flylint-temp-prefix
                               (file-name-nondirectory filename)))
             (tempfile (convert-standard-filename
                        (expand-file-name tempname
                                          (file-name-directory filename)))))
        (push tempfile flylint-temporaries)
        tempfile)
    (flylint--temp-file-system filename)))

(defun flylint--temp-unique-dir ()
  "Create a unique temp directory prefixed with `flylint-temp-prefix'.
Return directory will added to `flylint-temporaries'."
  (let* ((tempdir (make-temp-file flylint-temp-prefix 'directory)))
    (push tempdir flylint-temporaries)
    tempdir))


;;; Manage overlays

(defun flylint--get-buffer-point (line column)
  "Get buffer char point for `current-buffer' in LINE and COLUMN."
  (let ((inhibit-field-text-motion t))
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-min))
        (forward-line (- line 1))
        (cond
         ;; Line beyound EOF
         ;; Retern EOF
         ((eobp)
          (- (point-max) 1))
         ;; Empty line
         ;; Return previous line
         ((eolp)
          (line-end-position 0))
         ;; Default
         (t
          (let ((pos (min (+ (point) column)
                          (+ (line-end-position) 1))))
            (- pos 1))))))))

(defun flylint--add-overlay (err)
  "Add overlay for ERR."
  (flylint--debug 'promise-add-overlay
    (flylint-p-plist-to-string
     (list :error err)))
  (let ((buf))
    (cond
     ((not (flylint-error-p err))
      (flylint--debug :level :error 'promise-add-overlay
        (flylint-p-plist-to-string
         (list :error-msg "Expects ERR is object of `flylint-error', but not."
               :error (prin1-to-string err)))))
     ((not (setq buf (get-buffer (or (flylint-error-buffer err) (current-buffer)))))
      (flylint--debug :level :warning 'promise-add-overlay
        (flylint-p-plist-to-string
         (list :error-msg "The overlay was not added because target buffer is missing."
               :error (prin1-to-string err)
               :buffer (prin1-to-string buf)))))
     ((with-current-buffer buf
        (not flylint-mode))
      (flylint--debug :level :warning 'promise-add-overlay
        (flylint-p-plist-to-string
         (list :error-msg "The overlay was not added because `flylint-mode' is disabled."
               :error (prin1-to-string err)
               :buffer (prin1-to-string buf)))))
     (t
      (with-current-buffer buf
        (let* ((inhibit-field-text-motion t)
               (fringe-icon
                (lambda (level side)
                  (when side
                    (unless (memq side '(left-fringe right-fringe))
                      (error "Invalid fringe side: %S" side))
                    (propertize "!" 'display
                                (list side
                                      'flylint-fringe-double-arrow-bitmap
                                      (flylint--symbol 'fringe-face level))))))
               (region
                (save-restriction
                  (save-excursion
                    (widen)
                    (goto-char (flylint--get-buffer-point
                                (flylint-error-line err)
                                (flylint-error-column err)))
                    (bounds-of-thing-at-point
                     (or flylint-highlight-elements 'symbol)))))
               (ov     (when region (make-overlay (car region) (cdr region))))
               (level  (flylint-error-level err)))
          (when ov
            (overlay-put ov 'flylint-overlay t)
            (overlay-put ov 'flylint-error err)
            (overlay-put ov 'category (flylint--symbol 'ov-category level))
            (overlay-put ov 'face (when flylint-highlight-elements
                                    (flylint--symbol 'face level)))
            (overlay-put ov 'before-string (funcall fringe-icon level flylint-indication-fringe))
            (overlay-put ov 'help-echo (flylint-error-message err)))))))))

(defun flylint--overlays-in (beg end)
  "Get all flylint overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'flylint-overlay))
   (overlays-in beg end)))

(defun flylint--overlays-at (pos)
  "Get flylint overlays at POS."
  (apply #'flylint--overlays-in `(,pos ,pos)))

(defun flylint--remove-all-overlays ()
  "Remove all `flylint' overlays."
  (flylint--debug 'remove-all-overlays "nil")
  (save-restriction
    (widen)
    (mapc #'delete-overlay (flylint--overlays-in (point-min) (point-max)))))

(defun flylint--display-errors-in-echo-area (errors)
  "Display the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `display-message-or-buffer', which
shows the messages either in the echo area or in a separate
buffer, depending on the number of lines.  See Info
node `(elisp)Displaying Messages' for more information."
  (when (and errors
             (not (or cursor-in-echo-area (active-minibuffer-window))))
    (let ((formatfn (lambda (err)
                      (let ((filename (flylint-error-filename err))
                            (errormsg (flylint-error-message err))
                            (category (flylint-error-category err)))
                        (concat (when (and filename
                                           (not (equal filename (buffer-file-name))))
                                  (format "In \"%s\":\n  "
                                          (file-relative-name filename default-directory)))
                                errormsg
                                (when category
                                  (format " [%s]" category)))))))
      (display-message-or-buffer
       (mapconcat formatfn errors "\n")
       flylint-error-message-buffer
       'not-this-window))))

(defun flylint--display-errors-at-point-in-echo-area ()
  "Display error message at point in echo-area."
  (promise-chain (promise:delay flylint-display-errors-at-point-in-echo-area-delay)
    (then
     (lambda (_res)
       (when-let (ovs (flylint--overlays-at (point)))
         (flylint--display-errors-in-echo-area
          (mapcar (lambda (ov) (overlay-get ov 'flylint-error)) ovs)))))))


;;; Misc functioins

(defun flylint--locate-config-file (filename checker)
  "Locate the configuration file FILENAME for CHECKER.
Locate the configuration file using
`flylint--locate-config-file-functions'.

Return the absolute path of the configuration file, or nil if no
configuration file was found."
  (let ((filepath
         (run-hook-with-args-until-success 'flylint--locate-config-file-functions filename checker)))
    (if filepath
        (when (file-exists-p filepath)
          filepath))))

(defun flylint--prepend-with-option (option items &optional prepend-fn)
  "Prepend OPTION to each item in ITEMS, using PREPEND-FN.
Prepend OPTION to each item in ITEMS.

ITEMS is a list of strings to pass to the syntax checker.  OPTION
is the option, as string.  PREPEND-FN is a function called to
prepend OPTION to each item in ITEMS.  It receives the option and
a single item from ITEMS as argument, and must return a string or
a list of strings with OPTION prepended to the item.  If
PREPEND-FN is nil or omitted, use `list'.

Return a list of strings where OPTION is prepended to each item
in ITEMS using PREPEND-FN.  If PREPEND-FN returns a list, it is
spliced into the resulting list."
  (unless (stringp option)
    (error "Option %S is not a string" option))
  (unless prepend-fn
    (setq prepend-fn #'list))
  (let ((prepend
         (lambda (item)
           (let ((result (funcall prepend-fn option item)))
             (cond
              ((and (listp result) (seq-every-p #'stringp result)) result)
              ((stringp result) (list result))
              (t (error "Invalid result type for option: %S" result)))))))
    (apply #'append (seq-map prepend items))))

(defun flylint--save-buffer-to-file (filename)
  "Save the contents of the current buffer to FILENAME."
  (make-directory (file-name-directory filename) t)
  (let ((jka-compr-inhibit t))
    (write-region nil nil filename nil 0)))

(defun flylint--interpret-sexp (sexp &optional checker)
  "Interpret SEXP for CHECKER.
SEXP may be one of the following forms:

STRING
     Return ARG unchanged.

`source', `source-inplace'
     Create a temporary file to check and return its path.  With
     `source-inplace' create the temporary file in the same
     directory as the original file.  The value of
     `flylint-temp-prefix' is used as prefix of the file name.

     With `source', try to retain the non-directory component of
     the buffer's file name in the temporary file.

     `source' is the preferred way to pass the input file to a
     syntax checker.  `source-inplace' should only be used if the
     syntax checker needs other files from the source directory,
     such as include files in C.

`source-original'
     Return the path of the actual file to check, or an empty
     string if the buffer has no file name.

     Note that the contents of the file may not be up to date
     with the contents of the buffer to check.  Do not use this
     as primary input to a checker, unless absolutely necessary.

     When using this symbol as primary input to the syntax
     checker, add `flylint-buffer-saved-p' to the `:predicate'.

`temporary-directory'
     Create a unique temporary directory and return its path.

`temporary-file-name'
     Return a unique temporary filename.  The file is *not*
     created.

     To ignore the output of syntax checkers, try `null-device'
     first.

`null-device'
     Return the value of `null-device', i.e the system null
     device.

     Use this option to ignore the output of a syntax checker.
     If the syntax checker cannot handle the null device, or
     won't write to an existing file, try `temporary-file-name'
     instead.

`(config-file OPTION VARIABLE [PREPEND-FN])'
     Search the configuration file bound to VARIABLE with
     `flylint--locate-config-file' and return a list of arguments
     that pass this configuration file to the syntax checker, or
     nil if the configuration file was not found.

     PREPEND-FN is called with the OPTION and the located
     configuration file, and should return OPTION prepended
     before the file, either a string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

`(option OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE and return a list of
     arguments that pass this value as value for OPTION to the
     syntax checker.

     PREPEND-FN is called with the OPTION and the value of
     VARIABLE, and should return OPTION prepended before the
     file, either a string or as list.  If omitted, PREPEND-FN
     defaults to `list'.

     FILTER is an optional function to be applied to the value of
     VARIABLE before prepending.  This function must return nil
     or a string.  In the former case, return nil.  In the latter
     case, return a list of arguments as described above.

`(option-list OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE, which must be a list,
     and prepend OPTION before each item in this list, using
     PREPEND-FN.

     PREPEND-FN is called with the OPTION and each item of the
     list as second argument, and should return OPTION prepended
     before the item, either as string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

     FILTER is an optional function to be applied to each item in
     the list before prepending OPTION.  It shall return the
     option value for each item as string, or nil, if the item is
     to be ignored.

`(option-flag OPTION VARIABLE)'
     Retrieve the value of VARIABLE and return OPTION, if the
     value is non-nil.  Otherwise return nil.

`(eval FORM)'
     Return the result of evaluating FORM in the buffer to be
     checked.  FORM must either return a string or a list of
     strings, or nil to indicate that nothing should be
     substituted for CELL.  For all other return types, signal an
     error

     _No_ further substitutions are performed, neither in FORM
     before it is evaluated, nor in the result of evaluating
     FORM.

In all other cases, signal an error.

Note that substitution is *not* recursive.  No symbols or cells
are substituted within the body of cells!"
  (let ((fn (lambda (elm)
              (pcase elm
                ((pred stringp) (list elm))
                (`source
                 (list (flylint--temp-save-buffer #'flylint--temp-file-system)))
                (`source-inplace
                 (list (flylint--temp-save-buffer #'flylint--temp-file-inplace)))
                (`source-original (list (or (buffer-file-name) "")))
                (`temporary-directory (list (flylint--temp-unique-dir)))
                (`temporary-file-name
                 (let ((directory (flylint--temp-unique-dir)))
                   (list (make-temp-name (expand-file-name "flylint" directory)))))
                (`null-device (list null-device))
                (`(config-file ,option-name ,file-name-var)
                 (let ((value (symbol-value file-name-var)))
                   (when value
                     (let ((file-name (flylint--locate-config-file value checker)))
                       (when file-name
                         (flylint--prepend-with-option
                          option-name (list file-name)))))))
                (`(config-file ,option-name ,file-name-var ,prepend-fn)
                 (let ((value (symbol-value file-name-var)))
                   (when value
                     (let ((file-name (flylint--locate-config-file value checker)))
                       (when file-name
                         (flylint--prepend-with-option
                          option-name (list file-name) prepend-fn))))))
                (`(option ,option-name ,variable)
                 (let ((value (symbol-value variable)))
                   (when value
                     (unless (stringp value)
                       (error "Value %S of %S for option %s is not a string" value variable option-name))
                     (flylint--prepend-with-option option-name (list value)))))
                (`(option ,option-name ,variable ,prepend-fn)
                 (let ((value (symbol-value variable)))
                   (when value
                     (unless (stringp value)
                       (error "Value %S of %S for option %s is not a string" value variable option-name))
                     (flylint--prepend-with-option
                      option-name (list value) prepend-fn))))
                (`(option ,option-name ,variable ,prepend-fn ,filter)
                 (let ((value (funcall filter (symbol-value variable))))
                   (when value
                     (unless (stringp value)
                       (error "Value %S of %S (filter: %S) for option %s is not a string" value variable filter option-name))
                     (flylint--prepend-with-option
                      option-name (list value) prepend-fn))))
                (`(option-list ,option-name ,variable)
                 (let ((value (symbol-value variable)))
                   (unless (and (listp value) (seq-every-p #'stringp value))
                     (error "Value %S of %S for option %S is not a list of strings"
                            value variable option-name))
                   (flylint--prepend-with-option option-name value)))
                (`(option-list ,option-name ,variable ,prepend-fn)
                 (let ((value (symbol-value variable)))
                   (unless (and (listp value) (seq-every-p #'stringp value))
                     (error "Value %S of %S for option %S is not a list of strings"
                            value variable option-name))
                   (flylint--prepend-with-option option-name value prepend-fn)))
                (`(option-list ,option-name ,variable ,prepend-fn ,filter)
                 (let ((value (delq nil (seq-map filter (symbol-value variable)))))
                   (unless (and (listp value) (seq-every-p #'stringp value))
                     (error "Value %S of %S for option %S is not a list of strings"
                            value variable option-name))
                   (flylint--prepend-with-option option-name value prepend-fn)))
                (`(option-flag ,option-name ,variable)
                 (when (symbol-value variable)
                   (list option-name)))
                (`(eval ,form)
                 (let ((result (eval form)))
                   (cond
                    ((and (listp result) (seq-every-p #'stringp result)) result)
                    ((stringp result) (list result))
                    (t (error "Invalid result from evaluation of %S: %S" form result)))))
                (_ (error "Unsupported argument %S" elm))))))
    (mapcan fn sexp)))


;;; Management checker

(defun flylint--avairable-checkers ()
  "Get avairable checkers for BUF."
  (mapcar 'car
          (cl-remove-if-not
           (lambda (elm)
             (pcase-let ((`(_sym . ,checker) elm))
               (let ((modes (flylint-checker-modes checker))
                     (_fn   (flylint-checker-enabled checker)))
                 (apply #'derived-mode-p modes))))
           flylint-checker-alist)))

(defun flylint--avairable-checkers* ()
  "Get avairable checkers for BUF and considering :enabled."
  (mapcar 'car
          (cl-remove-if-not
           (lambda (elm)
             (pcase-let ((`(,_sym . ,checker) elm))
               (let ((modes (flylint-checker-modes checker))
                     (fn    (flylint-checker-enabled checker)))
                 (and (apply #'derived-mode-p modes)
                      (if fn (funcall fn) t)))))
           flylint-checker-alist)))

(defun flylint--running-p ()
  "Return non-nil if flylint running."
  flylint-running)

(defun flylint--promise-get-checker (checker)
  "Return promise to search CHECKER in `flylint-checker-alist'.

Promise will resolve CHECKER if exists.
Promise will reject if CHECKER missing with (missing-checker CHECKER)."
  (flylint--debug 'promise-get-checker
    (flylint-p-plist-to-string (list :checker checker)))
  (promise-new
   (lambda (resolve reject)
     (if (alist-get checker flylint-checker-alist)
         (funcall resolve checker)
       (funcall reject `(fail-get-checker ,checker))))))

(defun flylint--promise-exec-command (checker buffer)
  "Return promise to exec command for CHECKER and BUFFER.
If CHECKER's starndard-input is non-nil, send `current-buffer' to process.

Promise will resolve list such as (RETURN-CODE OUTPUT).
Promise will reject when command exit with unknown event (cannot parse
exit code.)"
  (let* ((checker* (flylint--get-checker checker))
         (cmd      (flylint--interpret-sexp
                    `(,(car (flylint-checker-command checker*))) checker))
         (cmd-args (flylint--interpret-sexp
                    (cdr (flylint-checker-command checker*)) checker))
         (stdin-p  (flylint-checker-standard-input checker*)))
    (let ((cmd*      (car cmd))
          (cmd-args* (append (cdr cmd) cmd-args))
          (exitcode  (lambda (str)
                       (when (stringp str)
                         (let ((reg "exited abnormally with code \\([[:digit:]]*\\)\n"))
                           (if (string-match reg str)
                               (string-to-number (match-string 1 str))
                             nil))))))
      (flylint--debug 'promise-exec-command
        (flylint-p-plist-to-string
         (list :checker checker
               :stdin-p stdin-p
               :cmd* cmd*
               :cmd-args* cmd-args*)))
      (promise-then
       (promise-race
        (vector
         (promise:time-out 10 'timeout)
         (with-current-buffer buffer
           (if stdin-p
               (apply #'promise:make-process-with-buffer-string
                      `(,cmd* ,buffer ,@cmd-args*))
             (apply #'promise:make-process
                    `(,cmd* ,@cmd-args*))))))
       (lambda (res)
         (promise-resolve `(0 ,(string-join res "\n"))))
       (lambda (reason)
         (let ((code (funcall exitcode (car-safe reason))))
           (if code
               (promise-resolve `(,code ,(string-join (cdr reason) "\n")))
             (promise-reject `(fail-exec (,cmd* ,@cmd-args*) ,reason)))))))))

(defun flylint--promise-tokenize-output (checker cmd-res)
  "Return promise to tokenize shell output CMD-RES for CHECKER.

Promise will resolve list of tokens as string.
Promise will reject when no-token but command doesn't exit code 0."
  (let ((checker* (flylint--get-checker checker)))
    (let ((compreg (flylint-checker-composed-error-pattern checker*))
          (regs    (flylint-checker-error-patterns checker*)))
      (flylint--debug 'promise-tokenize-output
        (flylint-p-plist-to-string
         (list :checker checker
               :return-code (nth 0 cmd-res)
               :stdout (nth 1 cmd-res)
               :stderr (nth 2 cmd-res))))
      (promise-then
       (promise:async-start
        `(lambda ()
           (let ((str ,(format "%s\n%s" (nth 1 cmd-res) (nth 2 cmd-res)))
                 (compreg ,compreg)
                 (regs ',regs)
                 (last-match 0)
                 ret)
             (while (string-match compreg str last-match)
               (push (match-string 0 str) ret)
               (setq last-match (match-end 0)))
             (nreverse ret))))
       (lambda (res)
         (if res
             (promise-resolve res)
           (unless (zerop (car cmd-res))
             (promise-reject `(fail-tokenize-nothing ,cmd-res)))))
       (lambda (reason)
         (promise-reject `(fail-tokenize-unknown ,reason ,cmd-res)))))))

(defun flylint--promise-parse-output (checker tokens)
  "Return promise to parse TOKENS for CHECKER.

Promise will resolve list of (TYPE BUFFER LINE COLUMN MESSAGE)
Promise will reject when fail child Emacs process."
  (let ((checker* (flylint--get-checker checker)))
    (let ((regs (flylint-checker-error-patterns checker*)))
      (flylint--debug 'promise-parse-output
        (flylint-p-plist-to-string
         (list :checker checker
               :tokens tokens)))
      (promise-then
       (promise:async-start
        `(lambda ()
           (let ((regs ',regs))
             (delq nil
                   (mapcan
                    (lambda (str)
                      (mapcar
                       (lambda (elm)
                         (let ((type (car elm))
                               (reg  (cdr elm)))
                           (when (string-match reg str)
                             `(,type                   ; level
                               ,(match-string 1 str)   ; filename
                               ,(match-string 2 str)   ; line
                               ,(match-string 3 str)   ; column
                               ,(match-string 4 str)   ; message
                               ,(match-string 5 str)   ; category
                               ))))
                       regs))
                    ',tokens)))))
       (lambda (res)
         (promise-resolve res))
       (lambda (reason)
         (promise-reject `(fail-parse-unknown ,reason ,tokens)))))))

(defun flylint--promise-add-overlay (checker errors)
  "Return promise to display ERRORS for CHECKER.
ERRORS format is return value `flylint--promise-parse-output'.

Promise will resolve with t if noerror.
Promise will reject when fail display ERRORS."
  (let ((safe-string-to-number (lambda (str)
                                 (let ((number-re
                                        (rx string-start (+ digit) string-end)))
                                   (if (and (stringp str)
                                            (string-match-p number-re str))
                                       (string-to-number str)
                                     0))))
        (safe-get-buffer (lambda (filename)
                           (if (and filename
                                    (get-buffer filename))
                               (get-buffer filename)
                             (current-buffer)))))
    (flylint--debug 'promise-add-overlay
      (flylint-p-plist-to-string
       (list :checker checker
             :errors errors)))
    (promise-new
     (lambda (resolve reject)
       (condition-case err
           (pcase-dolist (`(,level ,filename ,line ,column ,message ,category) errors)
             (flylint--add-overlay
              (flylint-error-new
               level
               (funcall safe-string-to-number line)
               (funcall safe-string-to-number column)
               message
               :checker checker
               :category category
               :filename filename
               :buffer (funcall safe-get-buffer filename))))
         (error
          (funcall reject `(fail-add-overlay ,errors ,err))))
       (funcall resolve t)))))

(async-defun flylint--run-checker (checker buffer)
  "Run CHECKER async for BUFFER."
  (flylint--debug 'run-checker
    (flylint-p-plist-to-string
     (list :checker checker
           :buffer (prin1-to-string buffer))))
  (condition-case err
      (let* ((res (await (flylint--promise-get-checker checker)))
             (res (await (flylint--promise-exec-command checker buffer)))
             (res (await (flylint--promise-tokenize-output checker res)))
             (res (await (flylint--promise-parse-output checker res)))
             (res (await (flylint--promise-add-overlay checker res)))))
    (error
     (pcase err
       (`(error (fail-get-checker ,checker))
        (flylint--warn "Missing checker.  checker: %s" checker))

       (`(error (fail-exec ,cmd ,reason))
        (flylint--warn "Cannot exec external program.
  checker: %s\n  command: %s\n  reason: %s" checker (prin1-to-string cmd) (prin1-to-string reason)))

       (`(error (fail-tokenize-nothing ,cmd-res))
        (flylint--warn "External program returned non-zero, but contained no errors.
  checker: %s\n  exit-code: %s\n  output: %s"
                       checker (nth 0 cmd-res) (nth 1 cmd-res)))

       (`(error (fail-tokenize-unknown ,reason ,cmd-res))
        (flylint--warn "External program exit normally, but cannot tokenize output.
  checker: %s\n  output: %s\n  reason: %s"
                       checker (nth 1 cmd-res) (prin1-to-string reason)))

       (`(error (fail-parse-unknown ,reason ,tokens))
        (flylint--warn "External program exit normally, and well tokenize, but cannot parse tokens.
  checker: %s\n  tokens: %s\n  reason: %s"
                       checker (prin1-to-string tokens) (prin1-to-string reason)))
       (`(error (fail-add-overlay ,errors ,err))
        (flylint--warn "External program exit normally, and well tokenize/parse, but cannot add overlay.
  checker: %s\n  errors: %s\n  reason: %s"
                       checker (prin1-to-string errors) (prin1-to-string err)))
       (_
        (flylint--warn "Something wrong while running checker.
  checker: %s\n  buffer: %s"
                       checker (prin1-to-string buffer)))))))

(defun flylint-run-checkers (triger &optional buffer)
  "Run checkers with TRIGER for BUFFER.
If omit buffer, run checkers for `current-buffer'.
see `flylint-check-syntax-triger'."
  (interactive (list 'manual))
  (let (buf)
    (cond
     ((not (setq buf (get-buffer (or buffer (current-buffer)))))
      (flylint--debug :break t :level :warning 'run-checkers
        (flylint-p-plist-to-string
         (list :error-msg "The running checker request was not executed because target buffer is missing."
               :triger triger
               :buffer (prin1-to-string (or buffer (current-buffer)))))))
     ((with-current-buffer buf
        (not flylint-mode))
      (with-current-buffer buf
        (flylint--debug :break t :level :warning 'run-checkers
          (flylint-p-plist-to-string
           (list :error-msg "The running checker request was not executed because `flylint-mode' is disabled."
                 :triger triger
                 :buffer (prin1-to-string buf)
                 :checkers flylint-enabled-checkers)))))
     ((with-current-buffer buf
        (flylint--running-p))
      (with-current-buffer buf
        (flylint--debug :break t :level :warning 'run-checkers
          (flylint-p-plist-to-string
           (list :error-msg "The running checker request was not executed because already checker executed."
                 :triger triger
                 :buffer (prin1-to-string buf)
                 :checkers flylint-enabled-checkers)))))
     ((with-current-buffer buf
        (not
         (or (eq 'manual triger)
             (memq triger flylint-check-syntax-triger))))
      (with-current-buffer buf
        (flylint--debug :break t :level :warning 'run-checkers
          (flylint-p-plist-to-string
           (list :error-msg "The running checker request was not executed because this triger is disabled."
                 :triger triger
                 :buffer (prin1-to-string buf)
                 :checkers flylint-enabled-checkers)))))
     (t
       (with-current-buffer buf
         (flylint--debug :break t 'run-checkers
           (flylint-p-plist-to-string
            (list :triger triger
                  :buffer buf
                  :checkers flylint-enabled-checkers)))
         (setq-local flylint-running t)
         (dolist (elm flylint-enabled-checkers)
           (flylint--run-checker elm buf))
         (setq-local flylint-running nil))))))

(defun flylint--handle-save ()
  "Handle a buffer save."
  (flylint-run-checkers 'save))

(defun flylint--handle-change (beg end _len)
  "Handle a buffer change between BEG to END.
BEG and END is mark at beggning and end change text.
_LEN is ignored.
Start a sntax check if newline has inserted into the buffer."
  (when flylint-mode
    (if (string-match-p "\n" (buffer-substring beg end))
        (flylint-run-checkers 'new-line)
      (flylint-run-checkers 'change))))

(defun flylint--handle-mode-enabled ()
  "Handle flylint-mode enabled."
  (flylint-run-checkers 'mode-enabled))


;;; Minor-mode support functions / variables

(defvar-local flylint-enabled-checkers nil
  "Syntax checkers to enabled for the current buffer.")

(defvar-local flylint-disabled-checkers nil
  "Syntax checkers to disabled for the current buffer.")

(defvar-local flylint-auto-disabled-checkers nil
  "Syntax checkers to disabled for the current buffer.")

(defvar-local flylint-temporaries nil
  "Temporary files and directories created.")

(defvar-local flylint-running nil
  "Non-nil if flylint running.")

(defun flylint--mode-lighter ()
  "Get a text describing status for use in the mode line."
  (let ((errc 0) (warnc 0))
    (format " %s:%s/%s" flylint-mode-line-prefix errc warnc)))

(defun flylint-mode--maybe ()
  "Enable `flylint-mode' if it is safe to do so.

Flylint-mode is enabled for
  - major modes listed in `flylint-enable-modes' (if t, enable all)

But Flylint-mode is not enabled for
  - the minibuffer
  - `fundamental-mode'
  - major modes whose `mode-class' property is `special'
  - ephemeral buffers (buffer name starts with space)
  - encrypted buffers (buffer has variable `epa-file-encrypt-to')
  - remote files (see `file-remote-p')
  - major modes listed in `flylint-disable-modes'
    (if nil, not disable all)"
  (when (and (or (eq t flylint-enable-modes)
                 (apply 'derived-mode-p flylint-enable-modes))
             (not (or (minibufferp)
                      (eq major-mode 'fundamental-mode)
                      (eq (get major-mode 'mode-class) 'special)
                      (string-prefix-p " " (buffer-name))
                      (local-variable-p 'epa-file-encrypt-to)
                      (and (buffer-file-name)
                           (file-remote-p (buffer-file-name) 'method))
                      (apply 'derived-mode-p flylint-disable-modes))))
    (flylint-mode)))

(defvar flylint-hooks-alist
  '(;; Handle events that may start automatic syntax checks
    (after-save-hook        . flylint--handle-save)
    (after-change-functions . flylint--handle-change)

    ;; Handle events that may triggered pending deferred checks
    ;; (window-configuration-change-hook . flylint-perform-deferred-syntax-check)
    ;; (post-command-hook                . flylint-perform-deferred-syntax-check)

    ;; Teardown Flylint whenever the buffer state is about to get lost, to
    ;; clean up temporary files and directories.
    (kill-buffer-hook       . flylint--teardown)
    (change-major-mode-hook . flylint--teardown)
    (before-revert-hook     . flylint--teardown)

    ;; Update the error list if necessary
    ;; (post-command-hook . flylint-error-list-update-source)
    ;; (post-command-hook . flylint-error-list-highlight-errors)

    ;; Display errors.  Show errors at point after commands (like movements) and
    ;; when Emacs gets focus.  Cancel the display timer when Emacs looses focus
    ;; (as there's no need to display errors if the user can't see them), and
    ;; hide the error buffer (for large error messages) if necessary.  Note that
    ;; the focus hooks only work on Emacs 24.4 and upwards, but since undefined
    ;; hooks are perfectly ok we don't need a version guard here.  They'll just
    ;; not work silently.
    (post-command-hook . flylint--display-errors-at-point-in-echo-area)
    (focus-in-hook     . flylint--display-errors-at-point-in-echo-area)
    ;; (focus-out-hook    . flylint-cancel-error-display-error-at-point-timer)
    ;; (post-command-hook . flylint-hide-error-buffer)

    ;; Immediately show error popups when navigating to an error
    ;; (next-error-hook . flylint-display-error-at-point)
    )
  "Hooks which Flylint needs to hook in.")

(defun flylint--setup ()
  "Setup flylint system."
  (setq-local flylint-running nil)
  (setq-local flylint-enabled-checkers (cl-set-difference
                                        (flylint--avairable-checkers*)
                                        flylint-global-disable-checkers))
  (setq-local flylint-auto-disabled-checkers (cl-set-difference
                                              (flylint--avairable-checkers)
                                              flylint-enabled-checkers))
  (setq-local flylint-disabled-checkers flylint-auto-disabled-checkers)
  (pcase-dolist (`(,hook . ,fn) flylint-hooks-alist)
    (add-hook hook fn nil 'local))

  ;; may run checkers with 'mode-enable argument
  (flylint--handle-mode-enabled))

(defun flylint--teardown ()
  "Teardown flylint system."
  (pcase-dolist (`(,hook . ,fn) flylint-hooks-alist)
    (remove-hook hook fn 'local))
  (flylint--remove-temporary-files)
  (flylint--remove-all-overlays))


;;; Main

(declare-function pkg-info-version-info "pkg-info")

;;;###autoload
(defun flylint-version ()
  "Get the Flylint version as string.
If called interactively, show the version in the echo area."
  (interactive)
  (let ((version (pkg-info-version-info 'flylint)))
    (if (called-interactively-p 'interactive)
        (message "Flylint version: %s" version)
      version)))

;;;###autoload
(define-minor-mode flylint-mode
  "Minor mode for asynchronous on-the-fly inspection."
  :keymap flylint-command-map
  :lighter (:eval (flylint--mode-lighter))
  :group 'flylint
  (if flylint-mode
      (flylint--setup)
    (flylint--teardown)))

;;;###autoload
(define-globalized-minor-mode global-flylint-mode flylint-mode
  flylint-mode--maybe
  :require 'flylint
  :group 'flylint)

(provide 'flylint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint.el ends here
