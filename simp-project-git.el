;;; simp-project-git.el --- Plugin to leverage git power in simp

;; Copyright (C) 2011-2015 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
;; Created: 02 Jun 2013
;; Version: 0.4.0
;; Keywords: project grep find git

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

(defun simp-project-is-git ()
  "Return non-nil if the current project is git revision controled"
  (simp-project-has-paths '(.git)))

(defun simp-project-find-files-generate-git-files-command ()
  "Git command to generate file list"
  (concat "git ls-files --full-name --cached --others --exclude-standard " (simp-project-root)))

(defun simp-project-git-grep-dwim ()
  "Use git-grep and grep mode to find matches"
  (let ((default-directory (concat (simp-project-root) "/")))
    (simp-project-git-grep-compilation (simp-project-rgrep-dwim-thing) "*")))

(defun simp-project-git-grep (&optional regexp pathspec dir)
  "Use git-grep and grep mode to find matches"
  (let* ((regexp (grep-read-regexp))
         (pathspec (grep-read-files pathspec))
         (default-directory (or dir (read-directory-name
                                     "Base directory: "
                                     nil default-directory t))))
    (simp-project-git-grep-compilation regexp pathspec)))

(defun simp-project-git-grep-compilation (regexp pathspec)
  (let ((flags "-InH --untracked")
        (case-fold-search nil))
    (unless (posix-string-match "[A-Z]" regexp)
      (set 'flags (concat flags " -i")))
    (compilation-start
     (concat "git --no-pager grep " flags " -e " (shell-quote-argument regexp) " -- '" pathspec "'")
     'grep-mode)))

(defadvice simp-project-find-files-generate-find-command (around simp-project-git-files activate)
  "Make simp-project-files to use git to find files for git projects."
  (if (simp-project-is-git)
      (setq ad-return-value (simp-project-find-files-generate-git-files-command))
    ad-do-it))

(defadvice simp-project-rgrep (around simp-project-git-grep activate)
  "Make simp-project-rgrep to use git to do the grepping for git projects."
  (if (simp-project-is-git)
      (simp-project-git-grep)
    ad-do-it))

(defadvice simp-project-rgrep-dwim (around simp-project-git-grep-dwim activate)
  "Make simp-project-rgrep-dwim to use git to do the grepping for git projects."
  (if (simp-project-is-git)
      (simp-project-git-grep-dwim)
    ad-do-it))

(provide 'simp-project-git)

;;; simp-project-git.el ends here
