;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit and push  -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013, 2014, 2015 Tom Willemse <tom@ryuslash.org>

;; Author: Tom Willemse <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 4.5.0
;; Keywords: vc
;; URL: http://projects.ryuslash.org/git-auto-commit-mode/

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

;; When `gac-debounce-interval' is non-nil and set to a number
;; representing seconds, it will only perform Git actions at that
;; interval. That way, repeatedly saving a file will not hammer the
;; Git repository.

;;; Code:

(defgroup git-auto-commit-mode nil
  "Customization options for `git-auto-commit-mode'."
  :group 'external)

(defcustom gac-automatically-push-p nil
  "Automatically push after each commit.

If non-nil a git push will be executed after each commit."
  :tag "Automatically push"
  :group 'git-auto-commit-mode
  :type 'boolean
  :risky t)
(make-variable-buffer-local 'gac-automatically-push-p)

(defcustom gac-automatically-add-new-files-p t
  "Should new (untracked) files automatically be committed to the repo?"
  :tag "Automatically add new files"
  :group 'git-auto-commit-mode
  :type 'boolean)

(defcustom gac-loaded-commit nil
  "Git commit in the buffer when it was loaded.

DO NOT MODIFY - used to ensure that the saved file doesn't conflict with
changes made since the current file was loaded."
  :tag "Current commit"
  :group 'git-auto-commit-mode
  :type 'string
  :risky t)
(make-variable-buffer-local 'gac-loaded-commit)

(defcustom gac-before-save-branch nil
  "Git branch active immediately before a file is saved.

DO NOT MODIFY - used to ensure that the saved file doesn't conflict with
changes made since the current file was loaded."
  :tag "Current commit"
  :group 'git-auto-commit-mode
  :type 'string
  :risky t)
(make-variable-buffer-local 'gac-before-save-branch)

(defcustom gac-merge-branch nil
  "Merge branch used when saving.

DO NOT MODIFY - used to ensure that the saved file doesn't conflict with
changes made since the current file was loaded."
  :tag "Current commit"
  :group 'git-auto-commit-mode
  :type 'string
  :risky t)
(make-variable-buffer-local 'gac-merge-branch)

(defcustom gac-ask-for-summary-p nil
  "Ask the user for a short summary each time a file is committed?"
  :tag "Ask for a summary on each commit"
  :group 'git-auto-commit-mode
  :type 'boolean)

(defcustom gac-shell-and " && "
  "How to join commands together in the shell. For fish shell,
  you want to customise this to: \" ; and \" instead of the default."
  :tag "Join shell commands"
  :group 'git-auto-commit-mode
  :type 'string)

(defcustom gac-add-additional-flag ""
       "Flag to add to the git add command."
       :tag "git add flag"
       :group 'git-auto-commit-mode
       :type 'string)


(defcustom gac-debounce-interval nil
  "Debounce automatic commits to avoid hammering Git.

If non-nil a commit will be scheduled to occur that many seconds
in the future. Note that this uses Emacs timer functionality, and
is subject to its limitations."
  :tag "Debounce interval"
  :group 'git-auto-commit-mode
  :type '(choice (number :tag "Interval in seconds")
                 (const :tag "Off" nil)))
(make-variable-buffer-local 'gac-debounce-interval)

(defcustom gac-default-message nil
  "Default message for automatic commits.

It can be:
- nil to use the default FILENAME
- a string which is used
- a function returning a string, called with FILENAME as
  argument, in which case the result is used as commit message
"
  :tag "Default commit message"
  :group 'git-auto-commit-mode
  :type '(choice (string :tag "Commit message")
                 (const :tag "Default: FILENAME" nil)
                 (function :tag "Function")))

(defun gac-relative-file-name (filename)
  "Find the path to FILENAME relative to the git directory."
  (let* ((git-dir
          (replace-regexp-in-string
           "\n+$" "" (gac--shell-command-to-string-throw
                      "git rev-parse --show-toplevel")))
         (relative-file-name
          (replace-regexp-in-string
           "^/" "" (replace-regexp-in-string
                    git-dir "" filename))))
    relative-file-name))

(defun gac-password (proc string)
  "Ask the user for a password when necessary.

PROC is the process running git.  STRING is the line that was
output by PROC."
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
  "Check if PROC is asking for a password and promps the user if so.

STRING is the output line from PROC."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (gac-password proc string))))

(defun gac-process-sentinel (proc status)
  "Report PROC change to STATUS."
  (message "git %s" (substring status 0 -1)))

(defun gac--commit-msg (filename)
  "Get a commit message.

Default to FILENAME."
  (let ((relative-filename (gac-relative-file-name filename)))
    (if (not gac-ask-for-summary-p)
        (if gac-default-message
            (if (functionp gac-default-message)
                (funcall gac-default-message filename)
              gac-default-message)
          relative-filename)
        (or gac-default-message relative-filename)
      (read-string "Summary: " nil nil relative-filename))))

(defun gac--current-commit ()
  "Return the current Git commit."
  (let* ((buffer-file (buffer-file-name))
         (default-directory (file-name-directory buffer-file)))
    (replace-regexp-in-string "\n\\'" ""
        (gac--shell-command-to-string-throw "git rev-parse --verify HEAD"))))

(defun gac--load-current-commit ()
  (setq-local gac-loaded-commit (gac--current-commit)))

(defun gac--current-branch ()
  "Return the current Git branch."
  (let* ((buffer-file (buffer-file-name))
         (default-directory (file-name-directory buffer-file)))
    (replace-regexp-in-string "\n\\'" ""
        (gac--shell-command-to-string-throw "git symbolic-ref --short HEAD"))))

(defun gac--shell-command-throw (command)
  "Run shell command, but raise a lisp error if the command returns nonzero.

Output and error printed to a temporary buffer."
  (let* ((buf (get-buffer-create "*gac shell command*"))
         (rv (call-process-shell-command command nil buf)))
    (unless (eql 0 rv)
      (error (concat "gac--shell-command-throw: "
                     "Exit code %d from command: %s")
             rv command))))

(defun gac--shell-command-to-string-throw (command)
  "Run shell command and return standard output as string.
but raise a lisp error if the command returns nonzero.

Standard error is inserted into a temp buffer if it's generated."
  (with-output-to-string
    (let ((err-file (make-temp-file "gac")))
      (unwind-protect
          (let* ((rv (call-process-shell-command
                      command nil (list standard-output err-file)))
                 (err-string (with-temp-buffer
                               (insert-file-contents err-file)
                               (buffer-string))))
            (when (not (string= "" err-string))
              (with-current-buffer-window
               "*gac shell command*" nil nil
               (insert err-string))
              (display-message-or-buffer err-string))
            (unless (eql 0 rv)
              (error (concat "gac--shell-command-to-string-throw: "
                             "Exit code %d from command: %s")
                     rv command)))
        (delete-file err-file)))))

(defun gac-checkout-merge-branch ()
  "Create and check out a merge branch."
  (setq-local gac-before-save-branch (gac--current-branch))
  (setq-local gac-merge-branch (format "gac-merge-%d" (float-time)))
  (let* ((buffer-file (buffer-file-name))
         (default-directory (file-name-directory buffer-file)))
    (gac--shell-command-throw
     (format "git checkout -b %s %s"
             (shell-quote-argument gac-merge-branch)
             (shell-quote-argument gac-before-save-branch)))))

(defun gac-commit (buffer)
  "Commit the current buffer's file to git."
  (let* ((buffer-file (buffer-file-name buffer))
         (filename (convert-standard-filename
                    (file-name-nondirectory buffer-file)))
         (commit-msg (gac--commit-msg buffer-file))
         (default-directory (file-name-directory buffer-file)))
    (gac--shell-command-throw
     (concat "git add " gac-add-additional-flag " " (shell-quote-argument filename)
             gac-shell-and
             "git commit -m " (shell-quote-argument commit-msg)))))

(defun gac-merge (buffer)
  "Merge gac-merge-branch back into gac-before-save-branch."
  (let* ((buffer-file (buffer-file-name))
         (default-directory (file-name-directory buffer-file)))
    (gac--shell-command-throw
     (concat (format "git checkout %s"
                     (shell-quote-argument gac-before-save-branch))
             gac-shell-and
             (format "git merge -m %s %s"
                     (shell-quote-argument
                      (format "Merge branch '%s'"
                              gac-merge-branch))
                     (shell-quote-argument gac-merge-branch))
             gac-shell-and
             (format "git branch -d %s"
                     (shell-quote-argument gac-merge-branch))))
    ;; Reset variables
    (setq-local gac-before-save-branch nil)
    (setq-local gac-merge-branch nil)
    (gac--load-current-commit)))

(defun gac-push (buffer)
  "Push commits to the current upstream.

This doesn't check or ask for a remote, so the correct remote
should already have been set up."
  ;; gac-push is currently only called from gac--after-save, where it is wrapped
  ;; in with-current-buffer, which should already take care of
  ;; default-directory. The explicit binding here is defensive, in case gac-push
  ;; starts being used elsewhere.
  (let ((default-directory (file-name-directory (buffer-file-name buffer))))
    (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
      (set-process-sentinel proc 'gac-process-sentinel)
      (set-process-filter proc 'gac-process-filter))))

(defvar gac--debounce-timers (make-hash-table :test #'equal))

(defun gac--debounced-save ()
  (let* ((actual-buffer (current-buffer))
         (current-buffer-debounce-timer (gethash actual-buffer gac--debounce-timers)))
    (unless current-buffer-debounce-timer
      (puthash actual-buffer
               (run-at-time gac-debounce-interval nil
                            #'gac--after-save
                            actual-buffer)
               gac--debounce-timers))))

(defun gac--buffer-is-tracked (buffer)
  "Check to see if BUFFER’s file is tracked in git."
  (let ((file-name (convert-standard-filename
                    (file-name-nondirectory
                     (buffer-file-name buffer)))))
    (not (string=
          (shell-command-to-string (concat "git ls-files " file-name))
          ""))))

(defun gac--buffer-has-changes (buffer)
  "Check to see if there is any change in BUFFER."
  (let ((file-name (convert-standard-filename
                    (file-name-nondirectory
                     (buffer-file-name buffer)))))
    (not (string=
          (shell-command-to-string (concat "git diff " file-name))
          ""))))

(defun gac--after-save (buffer)
  (unwind-protect
      (when (and (buffer-live-p buffer)
                 (or (and gac-automatically-add-new-files-p
                          (not (gac--buffer-is-tracked buffer)))
                     (gac--buffer-has-changes buffer)))
        (gac-commit buffer)
	(gac-merge buffer)
        (with-current-buffer buffer
          ;; with-current-buffer required here because gac-automatically-push-p
          ;; is buffer-local
          (when gac-automatically-push-p
            (gac-push buffer))))
    (remhash buffer gac--debounce-timers)))

(defun gac-kill-buffer-hook ()
  (when (and gac-debounce-interval
             gac--debounce-timers
             (gethash (current-buffer) gac--debounce-timers))
    (gac--after-save (current-buffer))))

(add-hook 'kill-buffer-hook #'gac-kill-buffer-hook)

(defun gac-find-file-func ()
  "Store the current commit."
  (gac--load-current-commit))

(defun gac-before-save-func ()
  "Create and check out a merge branch."
  (gac-checkout-merge-branch))

(defun gac-after-save-func ()
  "Commit the current file.

When `gac-automatically-push-p' is non-nil also push."
  (if gac-debounce-interval
      (gac--debounced-save)
    (gac--after-save (current-buffer))))

;;;###autoload
(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving with this
mode turned on and optionally push them too."
  :lighter " ga"
  (cond (git-auto-commit-mode
         (unless (null (buffer-file-name))
           (gac--load-current-commit)
           (add-hook 'find-file-hook 'gac-find-file-func t t)
           (add-hook 'before-save-hook 'gac-before-save-func t t)
           (add-hook 'after-save-hook 'gac-after-save-func t t)))
        (t
         (remove-hook 'find-file-hook 'gac-find-file-func t)
         (remove-hook 'before-save-hook 'gac-before-save-func t)
         (remove-hook 'after-save-hook 'gac-after-save-func t))))

(provide 'git-auto-commit-mode)

;;; git-auto-commit-mode.el ends here
