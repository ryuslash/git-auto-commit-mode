;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit and push  -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013, 2014, 2015 Tom Willemse <tom@ryuslash.org>

;; Author: Tom Willemse <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 4.7.0
;; Keywords: vc
;; URL: https://github.com/ryuslash/git-auto-commit-mode

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

(require 'subr-x)

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

(defcustom gac-commit-additional-flag ""
    "Flag to add to the git commit command."
    :tag "git commit flag"
    :group 'git-auto-commit-mode
    :type 'string)

(defcustom gac-silent-message-p nil
    "Should git output be output to the message area?"
    :tag "Quiet message output"
    :group 'git-auto-commit-mode
    :type 'boolean)


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
          (string-trim-right
           (shell-command-to-string "git rev-parse --show-toplevel"))))
    (file-relative-name filename git-dir)))

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

(defun gac-commit (buffer)
  "Commit the current buffer's file to git."
  (let* ((buffer-file (buffer-file-name buffer))
         (filename (convert-standard-filename
                    (file-name-nondirectory buffer-file)))
         (commit-msg (gac--commit-msg buffer-file))
         (default-directory (file-name-directory buffer-file)))
    (funcall (if gac-silent-message-p
                 #'call-process-shell-command
                 #'shell-command)
     (concat "git add " gac-add-additional-flag " " (shell-quote-argument filename)
             gac-shell-and
             "git commit -m " (shell-quote-argument commit-msg)
             " " gac-commit-additional-flag))))

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
  (if git-auto-commit-mode
      (add-hook 'after-save-hook 'gac-after-save-func t t)
    (remove-hook 'after-save-hook 'gac-after-save-func t)))

(provide 'git-auto-commit-mode)

;;; git-auto-commit-mode.el ends here
