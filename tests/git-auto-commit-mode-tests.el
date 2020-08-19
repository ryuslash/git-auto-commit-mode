;;; git-auto-commit-mode-tests.el --- Tests for git-auto-commit-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'buttercup)
(require 'git-auto-commit-mode)

(defun git-auto-commit-tests-setup-git ()
  (shell-command "git init")
  (shell-command "git config user.email user@example.com")
  (shell-command "git config user.name \"User Example\""))

(describe "New files"
  (describe "When ‘gac-automatically-add-new-files’ is t"
    (it "Should be added to git"
      (let* ((gac-automatically-add-new-files-p t)
             (temp-dir (make-temp-file "gac-" t))
             (temp-file (expand-file-name "test" temp-dir))
             (default-directory temp-dir))
        (unwind-protect
            (progn
              (git-auto-commit-tests-setup-git)
              (let ((buffer (find-file-noselect temp-file)))
                (with-current-buffer buffer
                  (git-auto-commit-mode)
                  (insert "test")
                  (save-buffer)))
              (expect (shell-command-to-string "git log --format=format:%s")
                      :to-match (rx string-start "test" string-end)))
          (delete-directory temp-dir t)))))

  (describe "When ‘gac-automatically-add-new-files’ is nil"
    (it "Shouldn’t be added to git"
      (let* ((gac-automatically-add-new-files-p nil)
             (temp-dir (make-temp-file "gac-" t))
             (temp-file (expand-file-name "test" temp-dir))
             (default-directory temp-dir))
        (unwind-protect
            (progn
              (git-auto-commit-tests-setup-git)
              (let ((buffer (find-file-noselect temp-file)))
                (with-current-buffer buffer
                  (git-auto-commit-mode)
                  (insert "test")
                  (save-buffer)))
              (expect (shell-command-to-string "git log --format=format:%s")
                      :not :to-match (rx string-start "test" string-end)))
          (delete-directory temp-dir t))))))

(describe "Getting a relative path"
  (it "should return the file name for files in the project root"
    (let* ((temp-dir (make-temp-file "gac-" t))
           (temp-file (expand-file-name "test" temp-dir))
           (default-directory temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "test"))
        (delete-directory temp-dir t))))

  (it "should return the file name and directory name for nested files"
    (let* ((temp-dir (make-temp-file "gac-" t))
           (temp-file (expand-file-name "test/test.txt" temp-dir))
           (default-directory temp-dir))
      (mkdir (expand-file-name "test" temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "test/test.txt"))
        (delete-directory temp-dir t))))

  (it "should handle ‘[]’ in the filename"
    (let* ((temp-dir (make-temp-file "gac-" t))
           (temp-file (expand-file-name "[test]-test.txt" temp-dir))
           (default-directory temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "[test]-test.txt"))
        (delete-directory temp-dir t))))

  (it "should handle ‘[]’ in the directory name"
    (let* ((temp-dir (make-temp-file "gac-" t))
           (temp-file (expand-file-name "[test]-test/testing.txt" temp-dir))
           (default-directory temp-dir))
      (mkdir (expand-file-name "[test]-test" temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "[test]-test/testing.txt"))
        (delete-directory temp-dir t))))

  (it "should handle ‘[’ in the directory name"
    (let* ((temp-dir (make-temp-file "gac-[test-" t))
           (temp-file (expand-file-name "testing.txt" temp-dir))
           (default-directory temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "testing.txt"))
        (delete-directory temp-dir t))))

  (it "should handle ‘[’ in the file name"
    (let* ((temp-dir (make-temp-file "gac-" t))
           (temp-file (expand-file-name "[test-test.txt" temp-dir))
           (default-directory temp-dir))
      (unwind-protect
          (progn
            (git-auto-commit-tests-setup-git)
            (let ((buffer (find-file-noselect temp-file)))
              (with-current-buffer buffer
                (git-auto-commit-mode)
                (insert "test")
                (save-buffer)))
            (expect (gac-relative-file-name temp-file)
                    :to-equal "[test-test.txt"))
        (delete-directory temp-dir t)))))

(provide 'git-auto-commit-mode-tests)
;;; git-auto-commit-mode-tests.el ends here
