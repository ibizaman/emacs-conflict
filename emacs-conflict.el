;;; emacs-conflict.el --- quickly find and resolve conflicts in external tools like Syncthing  -*- lexical-binding:t -*-

;; Copyright (C) 2019 Pierre Penninckx

;; Author: Pierre Penninckx <ibizapeanut@gmail.com>
;; URL: https://github.com/ibizaman/emacs-conflicts
;; Version: 0.1.0

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

;; `emacs-conflict' is used to quickly find and resolve conflicts in
;; external tools like Syncthing, Nextcloud or Pacman.
;;
;; `emacs-conflict-resolve-conflicts' searches a given directory for
;; all conflict files and provides a list of all of them.  The
;; inconvenience is this function works synchronously so it will block
;; Emacs.
;;
;; `emacs-conflict-show-conflicts-dired' is the asynchronous version
;; where the results are presented in a `dired' buffer thanks to
;; `find-name-dired'.  In the `dired' buffer, hover a conflict file
;; then call `emacs-conflict-resolve-conflict-dired'.
;;
;; In both cases, the conflict will be resolved using an `ediff'
;; session.
;;
;; The tool supports `Syncthing' [1], 'Nextcloud' [2] and 'Pacman' [3]
;; for now.
;;
;; [1] https://docs.syncthing.net/users/faq.html#what-if-there-is-a-conflict
;; [2] https://docs.nextcloud.com/desktop/2.6/conflicts.html
;; [3] https://wiki.archlinux.org/title/Pacman/Pacnew_and_Pacsave

;;; Code:


(require 'dired)
(require 'dired-aux)
(require 'ediff)


(defgroup conflict nil
  "Find conflicting files"
  :group 'files
  :prefix "emacs-conflict")

(defcustom emacs-conflict-find-regexes
  '(("syncthing" "\\.sync-conflict-[^.]*\\(\\.?\\)" "\\1")
    ("nextcloud" " (conflicted copy .*)\\(\\.?\\)" "\\1")
    ("pacman" "\\(?:\\.\\)pacnew$" ""))
  "Regexes to identify a file as a conflict."
  :type '(alist :key-type string :value-type (list regexp regexp))
  :group 'conflict)


(defun emacs-conflict--find-regexes ()
  "Merge all regexes into one."
  (mapconcat (lambda (ls) (nth 1 ls)) emacs-conflict-find-regexes "\\|"))

(defun emacs-conflict-resolve-conflicts (directory)
  "Resolve all conflicts under given DIRECTORY."
  (interactive "D")
  (let* ((all (emacs-conflict--get-sync-conflicts directory))
         (chosen (emacs-conflict--pick-a-conflict all)))
    (emacs-conflict--resolve-conflict chosen)))


(defun emacs-conflict-show-conflicts-dired (directory)
  "Open dired buffer at DIRECTORY showing all syncthing conflicts."
  (interactive "D")
  (find-lisp-find-dired directory (emacs-conflict--find-regexes)))


(defun emacs-conflict-resolve-conflict-dired (&optional arg)
  "Resolve conflict of first marked file in dired or close to point with ARG."
  (interactive "P")
  (let* ((chosen (car (dired-get-marked-files nil arg))))
    (emacs-conflict--resolve-conflict chosen)))


(defun emacs-conflict--resolve-conflict (conflict)
  "Resolve CONFLICT file using ediff."
  (let* ((normal (emacs-conflict--get-normal-filename conflict)))
    (emacs-conflict--resolve-ediff
     (list conflict normal)
     `(lambda ()
        (when (y-or-n-p "Delete conflict file? ")
          (kill-buffer (get-file-buffer ,conflict))
          (delete-file ,conflict))))))


(defun emacs-conflict--get-sync-conflicts (directory)
  "Return a list of all sync conflict files in a DIRECTORY."
  (directory-files-recursively directory (emacs-conflict--find-regexes)))


(defvar emacs-conflict--conflict-history nil
  "Completion conflict history.")

(defun emacs-conflict--pick-a-conflict (conflicts)
  "Let user choose the next conflict from CONFLICTS to investigate."
  (completing-read "Choose the conflict to investigate: " conflicts
                   nil t nil 'emacs-conflict--conflict-history))


(defun emacs-conflict--get-normal-filename (conflict)
  "Get non-conflict filename matching the given CONFLICT."
  (let (normal-filename)
	(dolist (r emacs-conflict-find-regexes normal-filename)
	  (let ((regex (nth 1 r))
			(replacement (nth 2 r)))
		(when (and
			   (null normal-filename)
			   (not (null (string-match-p regex conflict))))
		  (setq normal-filename
				(replace-regexp-in-string regex replacement conflict)))))))


(defun emacs-conflict--resolve-ediff (&optional files quit-hook)
  "Resolve conflict between files using `ediff'.

If FILES is nil, conflict resolution will be done between the two
marked files in `dired'.

QUIT-HOOK, if given is called ."
  (let ((files (or files (dired-get-marked-files)))
        (quit-hook quit-hook)
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (when quit-hook (funcall quit-hook))
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

(provide 'emacs-conflict)
;;; emacs-conflict.el ends here
