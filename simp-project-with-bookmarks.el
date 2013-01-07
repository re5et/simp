;;; simp-project-with-bookmark.el --- Utilize Emacs bookmarks with simp

;; Copyright (C) 2011-2013 @re5et

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

;;

(require 'simp-project)
(require 'bookmark)

(defmacro simp-project-with-bookmark (body)
  "Perform a simp project action using emacs
bookmark system to set the project context to use"
  `(let ((bookmark (list
                    (bookmark-completing-read
                     "Jump to bookmark"
                     bookmark-current-bookmark))))
     (let ((simp-buffer-project nil)
           (default-directory (bookmark-location (car bookmark))))
       ,body)))

(if (fboundp 'simp-project-find-file)
    (defun simp-project-with-bookmark-find-file ()
      "Simp project find file using simp project with bookmark"
      (interactive)
      (simp-project-with-bookmark
       (simp-project-find-file))))

(if (fboundp 'simp-project-rgrep)
    (defun simp-project-with-bookmark-rgrep ()
      "Simp project rgrep using simp project with bookmark"
      (interactive)
      (simp-project-with-bookmark
       (simp-project-rgrep))))

(provide 'simp-project-with-bookmarks)

;;; simp-project-with-bookmark.el ends here
