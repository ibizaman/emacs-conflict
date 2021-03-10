;;; emacs-conflict.el --- quickly find and resolve conflicts in external tools like Syncthing  -*- lexical-binding:t -*-

;; Copyright (C) 2019 Pierre Penninckx

;; Author: Pierre Penninckx <ibizapeanut@gmail.com>
;; URL: https://github.com/ibizaman/emacs-conflicts
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.0") (dash "2.11.0"))

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
;; external tools like Syncthing.
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
;; The tool only supports `Syncthing' [1] for now.
;;
;; [1] https://docs.syncthing.net/users/faq.html#what-if-there-is-a-conflict

;;; Code:


(require 'dired)
(require 'dired-aux)
(require 'ediff)
(require 'url)
(require 'cl)
(require 'dash)

(defgroup emacs-conflict nil
  "Handle conflicts when syncing files"
  :group 'external
  :prefix "emacs-conflict-")

(defcustom emacs-conflict-syncthing-url "http://localhost:8384/rest/"
  "The base URL for talking to Syncthing.

This should include the \"/rest/\" part."
  :type 'string)

(defcustom emacs-conflict-syncthing-api-keys nil
  "Lookup for the API key used i making requests to Syncthing.

Stored as an alist with the hostname as key and the API key as value. The alist
approach was chosen since it's not uncommon to use Syncthing to sync dotfiles
across multiple hosts. This avoids conflict in having a single key available for
all Syncthing instances.

The API key be obtained in the GUI in the Settings window."
  :type '(alist :key-type string :value-type string))

(defvar emacs-conflict-syncthing-conflict-marker ".sync-conflict"
  "The string used to denote that a file is a conflict file.")

(defvar emacs-conflict-syncthing-refresh-cache nil
  "Denotes if we should query Syncthing or use the cache.

Should only be set via `let' whenever we want to explcitly update the cache.
Avoid `setq' on this otherwise.")

(defun emacs-conflict--syncthing-require-version (version-string)
  "Check if VERSION-STRING is higher than the current installed version of Syncthing."
  (let* ((current-string
          ;; The substring removes the "v", since the emacs version- functions
          ;; don't like those.
          (substring
           (second
            (split-string (shell-command-to-string "syncthing -version") " "))
           1))
         (current (version-to-list current-string))
         (check (version-to-list version-string)))
    (i (version-list-< current check)
      (user-error "This feature requires Syncthing version %s and %s is installed. Please upgrade Syncthing to use this feature."
                  version-string
                  current-string))))

(defun emacs-conflict--get (url)
  "Make a GET request to the Syncthing API.

The object returned is the parsed JSON from the response."
  (let* ((url (concat emacs-conflict-syncthing-url url))
         (url-request-extra-headers (list (cons "X-API-key" (emacs-conflict-syncthing--api-key)))))
    ;; TODO(thiderman): Maybe some error handling if a non-200 response is returned?
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
        (goto-char (point-min))
        (re-search-forward "^$")
        (json-read)))))

(defun emacs-conflict-syncthing--api-key ()
  "Gets the API key for the current system, or requests it.

If requested, it is saved and persisted into `customize'."
  (let* ((key (alist-get (system-name) emacs-conflict-syncthing-api-keys)))
    (unless key
      (setq key (read-string "Syncthing API Key (obtain from Settings page in GUI): "))
      (customize-save-variable
       'emacs-conflict-syncthing-api-keys
       (acons (system-name) key emacs-conflict-syncthing-api-keys)))
    key))

(defun emacs-conflict-syncthing-roots ()
  "Lists all directories that are synced with Syncthing."
  ;; The /config/ endpoint was only made accessible via the API in version 1.12.
  ;; There are other ways of getting the roots, but this is by far the simplest
  ;; and most reliable one.
  (emacs-conflict--syncthing-require-version "1.12.0")
  (let* ((folders (plist-get (emacs-conflict--get "config") 'folders))
         ;; We're using `expand-file-name' here since Syncthing has a tendency
         ;; to sometimes use the ~ shorthand for $HOME and sometimes not.
         (roots (-map (lambda (f)
                        (expand-file-name (plist-get f 'path)))
                      folders)))
    (sort roots 'string-lessp)))

(defvar emacs-conflict-syncthing-conflicts-cache nil
  "Current conflict files in all syncthing folders.

This cache is used since finding conflicts in large (>GB) Syncthing folders can
be quite slow.")

(defun emacs-conflict-syncthing-conflicts ()
  "Gets all conflicts in Syncthing.

The result is put into `emacs-conflict-syncthing-conflicts-cache'. This cache is
  used unless empty or `emacs-conflict-syncthing-refresh-cache' is non-nil."
  (if (or emacs-conflict-syncthing-refresh-cache
          (not emacs-conflict-syncthing-conflicts-cache))
      (progn
        (message "Getting Syncthing conflicts...")
        (setq emacs-conflict-syncthing-conflicts-cache
              (-flatten
               (-map
                #'emacs-conflict--get-sync-conflicts
                (emacs-conflict-syncthing-roots))))
        (message "Conflict update complete.")))
  emacs-conflict-syncthing-conflicts-cache)

(defun emacs-conflict-syncthing-conflicts-refresh-cache ()
  "Update the contents of the conflict cache.

This always refreshes and returns `emacs-conflict-syncthing-conflicts-cache'."
  (interactive)
  (let ((emacs-conflict-syncthing-refresh-cache t))
    (emacs-conflict-syncthing-conflicts)))



(defun emacs-conflict-syncthing-conflict-file (filename)
  "Returns the conflict file for FILENAME if one is found in
`emacs-conflict-syncthing-conflicts'."
  ;; TODO(thiderman): Add support for finding multiple conflicts
  (cl-find filename (emacs-conflict-syncthing-conflicts)
           ;; Tests if the target file has the marker
           :test (lambda (target conflict)
                   (string-prefix-p
                    (concat (file-name-sans-extension target) emacs-conflict-syncthing-conflict-marker)
                    conflict))))

(defun emacs-conflict-act (func &optional directory)
  "Pick a conflict and call FUNC with it as only argument."
  (let* ((conflicts (if directory
                        (emacs-conflict--get-sync-conflicts directory)
                      (emacs-conflict-syncthing-conflicts)))
         (chosen (emacs-conflict--pick-a-conflict conflicts)))
    (funcall func chosen)))

(defun emacs-conflict-resolve-conflicts (&optional directory)
  "Resolve all conflicts under given DIRECTORY.

If no DIRECTORY is given, it defaults to all conflicts returned by
  `emacs-conflict-syncthing-conflicts' rather than in a specific directory."
  (interactive)
  (emacs-conflict-act #'emacs-conflict--resolve-conflict directory))

(defun emacs-conflict-remove-conflict (&optional directory)
  "Remove a conflict file.

If no DIRECTORY is given, it defaults to all conflicts returned by
  `emacs-conflict-syncthing-conflicts' rather than in a specific directory."
  (interactive)
  (emacs-conflict-act #'emacs-conflict--remove-conflict directory))


(defun emacs-conflict-show-conflicts-dired (directory)
  "Open dired buffer at DIRECTORY showing all syncthing conflicts.

If no directory is given, a prompt to select one from
`emacs-conflict-syncthing-roots' is shown."
  (interactive
   ;; TODO(thiderman): Only show roots that have conflicts?
   (list (completing-read "Syncthing root: " (emacs-conflict-syncthing-roots))))
  (find-name-dired directory (format "*%s-*" emacs-conflict-syncthing-conflict-marker)))


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
        (emacs-conflict--remove-conflict ,conflict)))))

(defun emacs-conflict--remove-conflict (conflict)
  "Deletes the conflict file and its buffer, and removes it from the conflict cache."
  (when (y-or-n-p (format "Delete conflict file %s? "
                          (file-name-nondirectory conflict)))
    ;; If there is no buffer, kill-buffer will be called with a nil argument,
    ;; which means kill the current buffer - not what we want.
    (if-let ((buf (get-file-buffer conflict)))
        (kill-buffer buf))
    (delete-file conflict)
    (setq emacs-conflict-syncthing-conflicts-cache
          (remove
           conflict
           emacs-conflict-syncthing-conflicts-cache))
    t))


(defun emacs-conflict--get-sync-conflicts (directory)
  "Return a list of all sync conflict files in a DIRECTORY."
  (directory-files-recursively directory
                               (format "%s-" (regexp-quote emacs-conflict-syncthing-conflict-marker))))


(defvar emacs-conflict--conflict-history nil
  "Completion conflict history")

(defun emacs-conflict--pick-a-conflict (conflicts)
  "Let user choose the next conflict from CONFLICTS."
  (completing-read "Choose conflict: " conflicts
                   nil t nil emacs-conflict--conflict-history))


(defun emacs-conflict--get-normal-filename (conflict)
  "Get non-conflict filename matching the given CONFLICT.

Tries to preserve the file extension."
  (replace-regexp-in-string
   (format "%s-.*\\(\\..*\\)$" (regexp-quote emacs-conflict-syncthing-conflict-marker)) "\\1" conflict))


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


(let ((map (define-prefix-command #'emacs-conflict-map)))
  (define-key map "c" #'emacs-conflict-resolve-conflicts)
  (define-key map "d" #'emacs-conflict-resolve-conflict-dired)
  (define-key map "r" #'emacs-conflict-syncthing-conflicts-refresh-cache)
  (define-key map "k" #'emacs-conflict-remove-conflict)
  map)

(provide 'emacs-conflict)
;;; emacs-conflict.el ends here
