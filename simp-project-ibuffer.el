;;; simp-project-ibuffer.el --- Find files in a simp project

;; Copyright (C) 2011-2015 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
;; Created: 16 Jan 2013
;; Version: 0.4.0
;; Keywords: project grep find

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA

;;; Commentary

(require 'ibuf-ext)

(define-ibuffer-filter simp-project-buffers
    "Ibuffer filtered by buffers associated with current-buffer simp-project"
  (:description "simp project"
                :reader (read-from-minibuffer "simp project directory: "))
  (ibuffer-awhen (simp-project-ibuffer-buffer-root buf)
    (string-match qualifier it)))

(define-ibuffer-filter simp-project-file-buffers
    "Ibuffer filtered by file buffers associated with current-buffer simp-project"
  (:description "simp project"
                :reader (read-from-minibuffer "simp project directory: "))
  (ibuffer-awhen (and (buffer-local-value 'buffer-file-name buf)
                      (simp-project-ibuffer-buffer-root buf))
    (string-match qualifier it)))

(defun simp-project-ibuffer (&optional files-only)
  "Start Ibuffer filtered to buffers associated with your
current buffers simp project.  If FILES-ONLY is non-nil, only
show buffers associated with files"
  (interactive)
  (let ((default-directory (simp-project-root))
        (simp-project-filter (if files-only
                                 'simp-project-file-buffers
                               'simp-project-buffers)))
    (ibuffer t "*simp Ibuffer*" `((,simp-project-filter . ,(simp-project-root))))))

(defun simp-project-ibuffer-files-only ()
  "Convenience method to start simp-project-ibuffer
with files only flag set."
  (interactive)
  (simp-project-ibuffer t))

(defun simp-project-ibuffer-buffer-root (buffer)
  (condition-case nil
      (with-current-buffer buffer (simp-project-root))
    (error nil)))

(simp-project-ibuffer-buffer-root (current-buffer))

(provide 'simp-project-ibuffer)
;;; simp-project-ibuffer.el ends here
