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
           '("tests/conflicts/one.sync-conflict-AABBCC.el"
			 "tests/conflicts/three.el.pacnew"
			 "tests/conflicts/two (conflicted copy 2021-05-28 225359).el"))))

(ert-deftest emacs-conflict-get-normal-file ()
  "Tests finding back the normal file from the conflict file."
  (should
	(equal
	 (mapcar 'emacs-conflict--get-normal-filename
			 '("tests/conflicts/one.sync-conflict-AABBCC.el"
			   "tests/conflicts/two (conflicted copy 2021-05-28 225359).el"
			   "tests/conflicts/three.el.pacnew"))
	 '("tests/conflicts/one.el"
	   "tests/conflicts/two.el"
	   "tests/conflicts/three.el"))))

(ert-deftest emacs-conflict-get-conflict-file ()
  "Tests finding back the normal file from the conflict file."
  (should
   (equal
    (mapcar (lambda (args) (let ((filename (nth 0 args))
                            (type (nth 1 args)))
                        (catch 'conflict-not-implemented
                          (emacs-conflict--get-conflict-filename filename type))))
            '(("tests/conflicts/one.el" "syncthing")
              ("tests/conflicts/two.el" "nextcloud")
              ("tests/conflicts/three.el" "pacman")))
    '("tests/conflicts/one.sync-conflict-.el"
      "tests/conflicts/two (conflicted copy 2021-05-28 225359).el"
      "tests/conflicts/three.el.pacnew"))))

(provide 'emacs-conflict-test)

;;; emacs-conflict-test.el ends here
