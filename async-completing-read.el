;;; async-completing-read.el --- An async-capable completing read function  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions

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

;; This package provides an `async-completing-read' function that
;; recognizes completion table with an `async' property in their
;; metadata and treats the specially. The value associated to the
;; `async' property should be a list of an executable program and
;; arguments for it. This program is started asynchronously and its
;; output collected in a buffer. The completion table can access the
;; output buffer by calling its predicate on the symbol
;; `output-buffer'.

;; A useful example of a class of async completion tables is provided:
;; (acr-lines-from-process prog args) returns a completion table for
;; which the completion candidates are lines the standard output of
;; prog run with arguments args.

;; As an example, once you have evaluated:

;; (setq completing-read-function #'async-completing-read)

;; you can prompt from any file found in the part of the directory
;; tree rooted at the current directory with the expression:

;; (completing-read "File: " (acr-lines-from-process "find" "."))

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup async-completing-read nil
  "An async-capable completing read function."
  :group 'completion)

(defcustom acr-refresh-completion-ui 'acr-refresh-icomplete
  "A function called to refresh the completion display."
  :type 'function
  :group 'async-completing-read)

(defcustom acr-refresh-completion-delay 0.3
  "How long to idle before refreshing completions.
Bear in mind that this only applies while idle: while the user is
typing completions are always fresh, as usual."
  :type 'number
  :group 'async-completing-read)

(defcustom acr-completing-read-function completing-read-function
  "Delegate actual `completing-read' work to this function.
The function `async-completing-read' really just handles
logistics of starting a process and getting lines from its stdout."
  :type 'function
  :group 'async-completing-read)

(defun async-completing-read
    (prompt collection &optional predicate &rest args)
  "Completing read function that recognizes asyc completion tables.
If the metadata fror COLLECTION specifies an `async' property, the
corresponding value is treated as a list of an executable program
and arguments for it. This function starts a process to run the
program, collects output in a buffer, refreshes the UI
periodically and delegates the completing-read work to
`acr-completing-read-function'. During completion the COLLECTION
can access the output buffer by the highly unorthodox means of
calling the predicate on the symbol `output-buffer'.

If the metadata has no async property, just call
`acr-completing-read-function' directly on COLLECTION."
  (if-let ((metadata (completion-metadata "" collection predicate))
           (async (completion-metadata-get metadata 'async))
           (output-buffer (generate-new-buffer "*async-completing-read*"))
           (update-timer (when acr-refresh-completion-ui
                           (run-with-timer
                            acr-refresh-completion-delay
                            acr-refresh-completion-delay
                            acr-refresh-completion-ui))))
      (unwind-protect
          (progn
            (apply
             #'start-process "*async-completing-read*" output-buffer async)
            (apply
             acr-completing-read-function prompt collection
             (lambda (candidate)
               (cond
                ((eq candidate 'output-buffer) output-buffer)
                ((functionp predicate) (funcall predicate candidate))
                (t t)))
             args))
        (when update-timer (cancel-timer update-timer))
        (kill-buffer output-buffer))
    (apply acr-completing-read-function prompt collection predicate args)))

(defun acr-lines-from-process (program &rest args)
  "Return a completion table for output lines from PROGRAM run with ARGS."
  (let ((last-pt 1) lines)
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (async ,program ,@args)
                     (category lines-from-process))
        (with-current-buffer (funcall pred 'output-buffer)
          (when (> (point-max) last-pt)
            (setq lines
                  (append lines
                          (split-string
                           (let ((new-pt (point-max)))
                             (prog1
                                 (buffer-substring last-pt new-pt)
                               (setq last-pt new-pt)))
                           "\n" 'omit-nulls)))))
          (complete-with-action action lines string pred)))))

(declare-function icomplete-exhibit "icomplete")

(defun acr-refresh-icomplete ()
  "Refresh icomplete completions."
  (when-let ((mini (active-minibuffer-window)))
    (with-selected-window mini
      (insert "@")
      (backward-delete-char 1)
      (icomplete-exhibit))))

(provide 'async-completing-read)
;;; async-completing-read.el ends here
