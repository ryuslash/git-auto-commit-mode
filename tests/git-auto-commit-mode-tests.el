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

(describe "New files"
  (describe "When ‘gac-automatically-add-new-files’ is t"
    (it "Should be added to git"
      (let* ((gac-automatically-add-new-files-p t)
             (temp-dir (make-temp-file "gac-" t))
             (temp-file (expand-file-name "test" temp-dir))
             (default-directory temp-dir))
        (unwind-protect
            (progn
              (shell-command "git init")
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
              (shell-command "git init")
              (let ((buffer (find-file-noselect temp-file)))
                (with-current-buffer buffer
                  (git-auto-commit-mode)
                  (insert "test")
                  (save-buffer)))
              (expect (shell-command-to-string "git log --format=format:%s")
                      :not :to-match (rx string-start "test" string-end)))
          (delete-directory temp-dir t))))))

(provide 'git-auto-commit-mode-tests)
;;; git-auto-commit-mode-tests.el ends here
