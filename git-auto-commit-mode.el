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
