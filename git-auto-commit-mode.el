;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 1
;; Keywords: vc
;; URL: http://ryuslash.org/git-auto-commit-mode/

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; git-auto-commit-mode is an Emacs minor mode that tries to commit
;; changes to a file after every save.

;;; Change Log:

;; 1 - Initial release.

;;; Code:

(defun git-auto-commit-relative-file-name (filename)
  "Find the path to the filename relative to the git directory"
  (let* ((git-dir
          (replace-regexp-in-string
           "\n+$" "" (shell-command-to-string
                      "git rev-parse --show-toplevel")))
         (relative-file-name
          (replace-regexp-in-string
           "^/" "" (replace-regexp-in-string
                    git-dir "" filename))))
    relative-file-name))

(defun git-auto-commit ()
  "Commit `buffer-file-name` to git"
  (let* ((filename (buffer-file-name))
         (relative-filename
          (git-auto-commit-relative-file-name filename)))
    (shell-command
     (concat "git add " filename
             " && git commit -m '" relative-filename "'"))))

(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving with this mode
turned on"
  :lighter " ga"
  (if git-auto-commit-mode
      (add-hook 'after-save-hook 'git-auto-commit t t)
    (remove-hook 'after-save-hook 'git-auto-commit t)))

;;; git-auto-commit-mode.el ends here
