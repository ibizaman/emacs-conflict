;;; emacs-conflict-init.el --- bare emacs-conflict init

;; Copyright (C) 2019 Pierre Penninckx

;; Author: Pierre Penninckx <ibizapeanut@gmail.com>
;; Maintainer: Pierre Penninckx <ibizapeanut@gmail.com>
;; URL: https://github.com/ibizaman/emacs-conflicts

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bare init file for Makefile's run target.

;;; Code:

(add-to-list 'load-path default-directory)
(mapc #'byte-compile-file '("emacs-conflict.el"))
(require 'emacs-conflict)
(global-set-key (kbd "C-c r r") 'emacs-conflict-resolve-conflicts)
(global-set-key (kbd "C-c r d") 'emacs-conflict-show-conflicts-dired)

(provide 'emacs-conflict-init)

;;; emacs-conflict-init.el ends here
