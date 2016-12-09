;;; my-ide.el --- development-related configuration

;;; Commentary:

;;; Code:


(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-global-mode)
  :config
    (setq
      projectile-cache-file (concat my-dir-projectile "projectile.cache")
      projectile-known-projects-file (concat my-dir-projectile "projectile-bookmarks.eld")
      projectile-enable-caching t
      projectile-switch-project-action 'neotree-projectile-action)
    (when (not (file-exists-p my-dir-projectile))
      (make-directory my-dir-projectile))
    (add-to-list 'projectile-other-file-alist '("cc" "h"))
    (add-to-list 'projectile-other-file-alist '("h" "cc")))

(use-package ido
  :ensure t
  :config
    (setq ido-enable-flex-matching t)
    (ido-mode 1))

(use-package semantic
  :ensure t
  :defer t
  :init (use-package semantic/bovine/gcc)
  :functions global-semanticdb-minor-mode global-semantic-idle-scheduler-mode
  :config
    (semantic-mode 1))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
    (global-company-mode)
    (setq semanticdb-default-save-directory my-dir-semantic)
    (when (not (file-exists-p semanticdb-default-save-directory))
      (make-directory semanticdb-default-save-directory)))

(use-package flycheck
  :ensure t
  :defer t
  :config (global-flycheck-mode))

(use-package which-func
  :commands which-function-mode
  :config (which-function-mode))

(use-package magit
  :ensure t
  :defer t)

(provide 'my-ide)
;;; my-ide.el ends here
