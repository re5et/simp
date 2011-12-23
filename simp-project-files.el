;;; simp-project-files.el --- Find files in a simp project

;; Copyright (C) 2011 @re5et

;; Author: atom smith
;; URL: http://trickeries.com
;; Created: 22 Dec 2011
;; Version: 0.1.1
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
;; files in a project without haveing to travese directories. I use:

;; (global-set-key (kbd "C-x M-f") 'simp-project-find-file)

;; You should also customize the variable simp-completing-read if you
;; want to use ido or something else to manage the completion

(require 'simp-project)

(defun simp-project-find-file ()
  "find file in project, excluding project's ignored paths,
using the unix find command for speedy results"
  (interactive)
  (find-file
   (format "%s/%s"
           (simp-project-root)
           (ido-completing-read
            "file: "
            (simp-project-files)))))

(defun simp-project-files ()
  "returns a list of files in a project, excluding project's
ignored paths, using the unix find command for speedy results"
  (split-string
   (shell-command-to-string
    (mapconcat
     'identity
     `("find"
       ,(simp-project-root)
       "\\("
       ,(format "-path \\*/%s" (car (simp-project-ignored)))
       ,(mapconcat (lambda (dir)
                     (format "-o -path \\*/%s" (symbol-name dir)))
                   (cdr (simp-project-ignored)) " ")
       "\\)"
       "-prune -o -type f"
       ,(format "| sed -E s:'%s/'::" (simp-project-root))
       ) " "))))

(provide 'simp-project-files)
