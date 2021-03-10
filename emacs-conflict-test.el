;;; emacs-conflict-test.el --- Tests for emacs-conflict
;; Copyright (C) 2019 Pierre Penninckx

;; Author: Pierre Penninckx <ibizapeanut@gmail.com>

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

;;; Code:

(ert-deftest emacs-conflict-find-conflict ()
  "Tests finding conflict file."
  (should (equal
           (mapcar 'file-relative-name (emacs-conflict--get-sync-conflicts "."))
           '("tests/conflicts/one.sync-conflict-AABBCC.el"))))

(ert-deftest emacs-conflict-get-normal-file ()
  "Tests finding back the normal file from the conflict file."
  (should (equal
           (emacs-conflict--get-normal-filename "tests/conflicts/one.sync-conflict-AABBCC.el")
           "tests/conflicts/one.el")))

(ert-deftest emacs-conflict-syncthing-conflict-file ()
  "Tests the inverse of the test above."
  (let ((emacs-conflict-syncthing-conflicts-cache
         '("tests/conflicts/one.sync-conflict-AABBCC.el")))
    (should (equal
             (emacs-conflict-syncthing-conflict-file "tests/conflicts/one.el")
             "tests/conflicts/one.sync-conflict-AABBCC.el"))))

(ert-deftest emacs-conflict--syncthing-require-version ()
  "Tests that the version checker works accordingly.

It's expected to error when the version is too low, and return nil otherwise."
  ;; Working current version
  (flet ((shell-command-to-string (x) "syncthing v1.12.0"))
    (should (null (emacs-conflict--syncthing-require-version "1.12.0"))))
  ;; Future version also works
  (flet ((shell-command-to-string (x) "syncthing v2.12.0"))
    (should (null (emacs-conflict--syncthing-require-version "1.12.0"))))
  ;; Error out when we're too low
  (flet ((shell-command-to-string (x) "syncthing v1.0.0"))
    (should-error (emacs-conflict--syncthing-require-version "1.12.0"))))

(provide 'emacs-conflict-test)

;;; emacs-conflict-test.el ends here
