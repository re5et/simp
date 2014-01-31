;;; simp-project-with-bookmarks.el --- Utilize Emacs bookmarks with simp

;; Copyright (C) 2011-2014 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
;; Created: 22 Dec 2011
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

;; Use your bookmarks to jump into a simp action without
;; having to go there first.  You can wrap any call in
;; simp-project-with-bookmark, and it will allow you to
;; select a bookmark to determine which project you are
;; working with.

(require 'simp-project)
(require 'bookmark)

(defmacro simp-project-with-bookmark (body)
  "Perform a simp project action using emacs
bookmark system to set the project context to use"
  `(let ((bookmark (list
                    (bookmark-completing-read
                     "bookmark for simp-project"
                     bookmark-current-bookmark))))
     (let ((simp-buffer-project nil)
           (default-directory (bookmark-location (car bookmark))))
       ,body)))

(defmacro simp-project-feature-with-bookmark (feature)
  (let ((with-bookmark-function-name (intern (format "simp-project-with-bookmark-%s" feature)))
        (function-name (intern (format "simp-project-%s" feature))))
    (if (fboundp function-name)
        `(defun ,with-bookmark-function-name ()
           ,(format "%s using simp project with bookmark" function-name)
           (interactive)
           (simp-project-with-bookmark
            (,function-name))))))

;; Make simp-project-with-bookmark convenience methods
;; for included simp project features

(simp-project-feature-with-bookmark rgrep)
(simp-project-feature-with-bookmark find-file)
(simp-project-feature-with-bookmark ibuffer)

(provide 'simp-project-with-bookmarks)

;;; simp-project-with-bookmarks.el ends here
