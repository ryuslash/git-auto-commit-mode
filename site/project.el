(require 'org-publish)

(setq org-publish-project-alist
      '(("git-auto-commit-mode-files"
         :base-directory "./"
         :publishing-directory "_publish/"
         :recursive nil
         :base-extension "css"
         :publishing-function org-publish-attachment)
        ("git-auto-commit-mode-org"
         :base-directory "./"
         :publishing-directory "_publish/"
         :recursive nil
         :base-extension "org"
         :publishing-function org-publish-org-to-html)
        ("git-auto-commit-mode-site"
         :components ("git-auto-commit-mode-org"
                      "git-auto-commit-mode-files"))))
