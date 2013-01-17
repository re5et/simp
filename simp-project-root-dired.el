;;; simp-project-root-dired.el --- Quickly jump to project root, dired

;; Copyright (C) 2011-2013 @re5et

;; Author: atom smith
;; URL: https://github.com/re5et/simp
;; Created: 01 Jan 2013
;; Version: 0.4.0
;; Keywords: project dired

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

(require 'simp-project)

(defun simp-project-root-dired ()
  "Start dired at the current buffers associated simp project root"
  (interactive)
  (dired (simp-project-root)))

(provide 'simp-project-root-dired)
