;;; my-ide.el --- development-related configuration

;;; Commentary:

;;; Code:


;; keybindings
(bind-keys :prefix-map my-prefix-code
	   :map my-prefix
	   :prefix "c"
	   ("c" . compile)
	   ("w" . global-whitespace-mode))

;; packages

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-global-mode projectile-compile-project)
  :init (projectile-global-mode)
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
  :bind (:map my-prefix
              ([?\t] . company-complete-common))
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
  :bind (:map my-prefix-code
	      ("g" . magit-status)))

(setq compilation-scroll-output 'first-error)
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

(provide 'my-ide)
;;; my-ide.el ends here
