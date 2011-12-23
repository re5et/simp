;;; simp.el --- Simple project defenition, chiefly for file finding, and grepping

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

;;; Commentary

;; If you want to use any or some of these without everything, require
;; just the things you want.

(require 'simp-project)
(require 'simp-project-files)
(require 'simp-project-rgrep)

(provide 'simp)
