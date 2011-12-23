;;; simp-project.el --- Simple project defenition, chiefly for file finding, and grepping

;; Copyright (C) 2011 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
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

;;; Commentary:

;; A very simple generic project defenition to work with git projects:

;; (simp-project-define
;;  '(:has (.git)
;;    :ignore (.git)))

;; A buffer will match as a member of the above simp project if it's associated
;; file has an ancestor directory that contains a .git file or directory, and that
;; when dealing with files in this project, the .git file or directory should be
;; excluded because you don't care about it.

;; You could likewise work with generic mercurial project like:

;; (simp-project-define
;;  '(:has (.hg)
;;    :ignore (.hg)))

;; A more complex project defenition might look like this:

;; (simp-project-define
;;  '(:type rails
;;    :has (config.ru app/views app/models app/controllers)
;;    :ignore (tmp coverage log vendor .git public/system public/assets)))

;; For a buffer to match as a member of this project, it must be a descendant of
;; some directory that contains each path in the :has list.

;; As above, the list of paths in the :ignore property will be excluded when
;; dealing with project files

;; In this example, ':type' is not actually going to be used by built-in simp
;; functions, but rather would be used to extend projects that match the ':has'
;; criteria.  Like all properties, a buffer's project's ':type' can be accessed
;; using (simp-project-get :type)

;; I use the following to work in my .emacs.d directory:

;; (simp-project-define
;;  '(:type emacs
;;    :has (init.el)))

(defvar simp-projects ())
(defvar simp-buffer-project nil)
(make-variable-buffer-local 'simp-buffer-project)

(defcustom simp-completing-read-command 'completing-read
  "The completing read command simp-completing-read will use.")

(defun simp-completing-read (prompt collection)
  "Internal simp use, completing read used by simp.
Uses custom variable simp-completing-read-command. PROMPT
will be used as the text shown on the minibuffer completion,
COLLECTION is the list of possible completions."
  (funcall simp-completing-read-command prompt collection))

(defun simp-project-for-current-buffer ()
  "Returns the project the current buffer
is associated with.  If the buffer local variable
is set, simply return it, otherwise determine the
correct project and set it"
  (or simp-buffer-project
      (dolist (project simp-projects)
        (let* ((paths (plist-get project :has))
               (found-project (simp-project-has-paths paths)))
          (if found-project
              (progn
                (plist-put project :root (directory-file-name found-project))
                (return (setq simp-buffer-project project))))))))

(defun simp-project-has-paths (paths &optional dir)
  "Used to match a path to a project.  PATHS are tested
to see if they exist in DIR"
  (let ((dir (or dir default-directory)))
    (if (member
         nil
         (mapcar
          (lambda (path)
            (file-exists-p (expand-file-name (symbol-name path) dir)))
          paths))
        (unless (string= dir "/")
          (simp-project-has-paths paths (expand-file-name ".." dir)))
      dir)))

(defun simp-project-get (member)
  "get MEMBER property from the current project"
  (plist-get
   (simp-project-for-current-buffer)
   member))

(defun simp-project-root ()
  "get the current buffers project root"
  (simp-project-get :root))

(defun simp-project-ignored ()
  "get the current buffers project ignored paths"
  (simp-project-get :ignore))

(defun simp-project-type ()
  "get the current buffer project type"
  (simp-project-get :type))

(defun simp-project-define (project)
  "define a simp project. PROJECT should be a plist, which can contain any
number of key value pairs that you wish to reference using simp-project-get."
  (push project simp-projects))

(provide 'simp-project)
