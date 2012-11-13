;;; simp-project-files.el --- Find files in a simp project

;; Copyright (C) 2011-2012 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
;; Created: 22 Dec 2011
;; Version: 0.2.0
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

;; Bind simp-project-find-file to something so you can quickly narrow down
;; files in a project without having to traverse directories. I use:

;; (global-set-key (kbd "C-x M-f") 'simp-project-find-file)

;; You should also customize the variable simp-completing-read if you
;; want to use ido or something else to manage the completion

(require 'simp-project)

(defcustom simp-project-find-file-sort-command
  'simp-project-find-file-sort-short-filename
  "The command to sort the found files returned by simp-project-find-file"
  :group 'simp)

(defun simp-project-find-file ()
  "find file in project, excluding project's ignored paths,
using the unix find command for speedy results"
  (interactive)
  (find-file
   (format "%s/%s"
           (simp-project-root)
           (simp-completing-read
            "file: "
            (simp-project-files)))))

(defun simp-project-files ()
  "Returns a sorted list of files in a project, excluding project's
ignored paths, using the unix find command for speedy results.
Set simp-project-find-file-sort-command to the command you want to sort with"
  (sort (simp-project-get-files) simp-project-find-file-sort-command))

(defun simp-project-get-files ()
  "Returns a list of files in a project, excluding project's
ignored paths, using the unix find command for speedy results."
  (let ((find-command (simp-project-find-files-generate-find-command)))
    (split-string
     (shell-command-to-string find-command)
     "\n" t)))

(defun simp-project-find-files-generate-find-command ()
  (let ((project-root (expand-file-name (simp-project-root))))
    (mapconcat
     'identity
     `("find"
       ,project-root
       "\\("
       ,(format "-path \\*/%s" (car (simp-project-ignored)))
       ,(mapconcat (lambda (dir)
                     (format "-o -path \\*/%s" (symbol-name dir)))
                   (cdr (simp-project-ignored)) " ")
       "\\)"
       "-prune -o -type f"
       ,(format "| sed -E s:'%s/'::" project-root)
       ) " ")))

(defun simp-project-find-file-sort-short-filename (a b)
  "Sort files by filename, shortest to longest.  This is currently
the default. To use, set simp-project-find-file-sort-command to
simp-project-find-file-sort-short-filename."
  (< (length a) (length b)))

(defun simp-project-find-file-sort-modified-time (a b)
  "Sort files by file modified time, most recent to longest ago.
 To use, set simp-project-find-file-sort-command to
simp-project-find-file-sort-modified-time."
  (time-less-p
   (sixth (file-attributes b))
   (sixth (file-attributes a))))

(provide 'simp-project-files)
