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

(defun git-auto-commit-mode-tests--get-last-commit-message ()
  "Get the last commit message from git."
  (shell-command-to-string "git log --format=format:%s"))

(describe "In a git repository"
  :var (temp-dir current-directory)

  (before-each
    (setq temp-dir (make-temp-file "gac-" t)
          current-directory default-directory
          default-directory temp-dir)
    (shell-command "git init --initial-branch=gac-test")
    (shell-command "git config user.email user@example.com")
    (shell-command "git config user.name \"User Example\""))

  (after-each
    (delete-directory temp-dir t)
    (setq temp-dir nil
          default-directory current-directory
          current-directory nil))

  (describe "New files"
    (describe "When ‘gac-automatically-add-new-files’ is t"
      (it "Should be added to git"
        (let* ((gac-automatically-add-new-files-p t)
               (temp-file (expand-file-name "test" temp-dir))
               (buffer (find-file-noselect temp-file)))
          (with-current-buffer buffer
            (git-auto-commit-mode)
            (insert "test")
            (save-buffer))

          (expect (git-auto-commit-mode-tests--get-last-commit-message)
                  :to-match (rx string-start "test" string-end)))))

    (describe "When ‘gac-automatically-add-new-files’ is nil"
      (it "Shouldn’t be added to git"
        (let* ((gac-automatically-add-new-files-p nil)
               (temp-file (expand-file-name "test" temp-dir))
               (buffer (find-file-noselect temp-file)))
          (with-current-buffer buffer
            (git-auto-commit-mode)
            (insert "test")
            (save-buffer))

          (expect (git-auto-commit-mode-tests--get-last-commit-message)
                  :not :to-match (rx string-start "test" string-end))))))

  (describe "Getting a relative path"
    (it "should return the file name for files in the project root"
      (let* ((temp-file (expand-file-name "test" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "test")))

    (it "should return the file name and directory name for nested files"
      (mkdir (expand-file-name "test" temp-dir))

      (let* ((temp-file (expand-file-name "test/test.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "test/test.txt")))

    (it "should handle ‘[]’ in the filename"
      (let* ((temp-file (expand-file-name "[test]-test.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "[test]-test.txt")))

    (it "should handle ‘[]’ in the directory name"
      (mkdir (expand-file-name "[test]-test" temp-dir))

      (let* ((temp-file (expand-file-name "[test]-test/testing.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "[test]-test/testing.txt")))

    (it "should handle ‘[’ in the directory name"
      (let* ((temp-file (expand-file-name "testing.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "testing.txt")))

    (it "should handle ‘[’ in the file name"
      (let* ((temp-file (expand-file-name "[test-test.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (gac-relative-file-name temp-file)
                :to-equal "[test-test.txt"))))

  (describe "Getting a commit message"
    (it "should default to the relative file name"
      (let* ((temp-file (expand-file-name "test.txt" temp-dir))
             (buffer (find-file-noselect temp-file)))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (git-auto-commit-mode-tests--get-last-commit-message)
                :to-equal "test.txt")))

    (it "should use the message specified in ‘gac-default-message’"
      (let* ((temp-file (expand-file-name "test.txt" temp-dir))
             (buffer (find-file-noselect temp-file))
             (gac-default-message "I made a commit!"))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (git-auto-commit-mode-tests--get-last-commit-message)
                :to-equal "I made a commit!")))

    (it "should use the function result if ‘gac-default-message’ is a function"
      (let* ((temp-file (expand-file-name "test.txt" temp-dir))
             (buffer (find-file-noselect temp-file))
             (gac-default-message
              (lambda (f)
                (concat "wip " (gac-relative-file-name f)))))
        (with-current-buffer buffer
          (git-auto-commit-mode)
          (insert "test")
          (save-buffer))

        (expect (git-auto-commit-mode-tests--get-last-commit-message)
                :to-equal "wip test.txt")))))

(provide 'git-auto-commit-mode-tests)
;;; git-auto-commit-mode-tests.el ends here
