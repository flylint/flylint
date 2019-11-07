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


;;; promise
(defun promise:make-process-with-buffer-string (program buf &rest args)
  "Generate an asynchronous process and return Promise to resolve
with (stdout stderr) on success and with (event stdout stderr) on error
with stdin `buffer-string' of BUF."
  (promise-new
   (lambda (resolve reject)
     (let* ((stdout (generate-new-buffer (concat "*" program "-stdout*")))
            (stderr (generate-new-buffer (concat "*" program "-stderr*")))
            (stderr-pipe (make-pipe-process
                          :name (concat "*" program "-stderr-pipe*")
                          :noquery t
                          ;; use :filter instead of :buffer, to get rid of "Process Finished" lines
                          :filter (lambda (_ output)
                                    (with-current-buffer stderr
                                      (insert output)))))
            (cleanup (lambda ()
                       (delete-process stderr-pipe)
                       (kill-buffer stdout)
                       (kill-buffer stderr))))
       (condition-case err
           (let ((proc (make-process :name program
                                     :buffer stdout
                                     :command (cons program args)
                                     :stderr stderr-pipe
                                     :sentinel (lambda (process event)
                                                 (unwind-protect
                                                     (let ((stderr-str (with-current-buffer stderr (buffer-string)))
                                                           (stdout-str (with-current-buffer stdout (buffer-string))))
                                                       (if (string= event "finished\n")
                                                           (funcall resolve (list stdout-str stderr-str))
                                                         (funcall reject (list event stdout-str stderr-str))))
                                                   (funcall cleanup))))))
             (with-current-buffer buf
               (process-send-region proc (point-min) (point-max))
               (process-send-eof proc)))
         (error (funcall cleanup)
                (signal (car err) (cdr err))))))))

(defun promise:make-process-with-string (program string &rest args)
  "Generate an asynchronous process and return Promise to resolve
with (stdout stderr) on success and with (event stdout stderr) on error
with stdin `buffer-string' of BUF."
  ;; reference `with-temp-buffer'
  (let ((temp-buffer (generate-new-buffer " *temp*")))
    (with-current-buffer temp-buffer
      (unwind-protect
          (progn
            (insert (substring-no-properties string))
            (apply #'promise:make-process-with-buffer-string `(,temp-buffer ,@args)))
        (and (buffer-name temp-buffer)
             (kill-buffer temp-buffer))))))

(provide 'flylint-polyfill)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; flylint-polyfill.el ends here
