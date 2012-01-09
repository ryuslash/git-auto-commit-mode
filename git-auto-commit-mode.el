(defun git-auto-commit ()
  "Commit `buffer-file-name` to git"
  (let ((filename (buffer-file-name)))
    (shell-command (concat "git add " filename
                           " && git commit -m '" filename "'"))))

(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving with this mode
turned on"
  :lighter "ga"
  (if git-auto-commit-mode
      (add-hook 'after-save-hook 'git-auto-commit t t)
    (remove-hook 'after-save-hook 'git-auto-commit t)))
