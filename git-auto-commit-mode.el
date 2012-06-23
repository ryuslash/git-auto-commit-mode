;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit and push

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 4.1
;; Keywords: vc
;; URL: http://org.ryuslash.org/projects/git-auto-commit-mode.html

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

;; When `gac-automatically-push-p' is non-nil, it also tries to push
;; to the current upstream.

;;; Change Log:

;; 1 - Initial release.

;; 2 - Add ability to automatically push.

;; 3 - Shows the status when push finishes.

;; 4 - Make `gac-automatically-push' buffer local, always.

;;   - Rename `gac-automatically-push' to `gac-automatically-push-p'
;;     to follow standard emacs-lisp conventions.

;;   - Fix commentary about `gac-automatically-push-p'.

;;; Code:

(defvar gac-automatically-push-p nil
  "Control whether or not `git-auto-commit-mode' should also
  automatically push the changes.")
(make-variable-buffer-local 'gac-automatically-push-p)

(defun gac-relative-file-name (filename)
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

(defun gac-password (proc string)
  "Ask the user for a password when necessary."
  (let (ask)
    (cond
     ((or
       (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
       (string-match "^\\\(.*\\\)'s password:" string))
      (setq ask (format "Password for '%s': " (match-string 1 string))))
     ((string-match "^[pP]assword:" string)
      (setq ask "Password:")))

    (when ask
      (process-send-string proc (concat (read-passwd ask nil) "\n")))))

(defun gac-process-filter (proc string)
  "Checks if the process is asking for a password and asks the
user for one when it does."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (gac-password proc string))))

(defun gac-process-sentinel (proc status)
  "Report the process' status change."
  (message "git %s" (substring status 0 -1)))

(defun gac-commit ()
  "Commit `buffer-file-name' to git"
  (let* ((filename (buffer-file-name))
         (relative-filename
          (gac-relative-file-name filename)))
    (shell-command
     (concat "git add " filename
             " && git commit -m '" relative-filename "'"))))

(defun gac-push ()
  "Push changes to the repository to the current upstream. This
doesn't check or ask for a remote, so the correct remote should
already have been set up."
  (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
    (set-process-sentinel proc 'gac-process-sentinel)
    (set-process-filter proc 'gac-process-filter)))

(defun gac-after-save-func ()
  "Commit the changes to the current file, and when
`gac-automatically-push-p' is not `nil', push."
  (gac-commit)
  (when gac-automatically-push-p
    (gac-push)))

;;;###autoload
(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving with this
mode turned on and optionally push them too."
  :lighter " ga"
  (if git-auto-commit-mode
      (add-hook 'after-save-hook 'gac-after-save-func t t)
    (remove-hook 'after-save-hook 'gac-after-save-func t)))

;;; git-auto-commit-mode.el ends here
