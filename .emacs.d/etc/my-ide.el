; quickly access a project's files
 (require 'projectile)
 (setq
    projectile-cache-file (concat my-dir-projectile "projectile.cache")
    projectile-known-projects-file (concat my-dir-projectile "projectile-bookmarks.eld"))
 (when (not (file-exists-p my-dir-projectile))
   (make-directory my-dir-projectile))
 (projectile-global-mode +1)
 (setq projectile-enable-caching t)
(add-to-list 'projectile-other-file-alist '("cc" "h"))
(add-to-list 'projectile-other-file-alist '("h" "cc"))

; (require 'helm-config)
; (helm-mode 1)
; (setq projectile-completion-system 'helm)
; (helm-projectile-on)
; (setq projectile-switch-project-action 'helm-projectile)

; enable completion
(require 'semantic)
(require 'semantic/bovine/gcc)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)

(setq semanticdb-default-save-directory my-dir-semantic)
(when (not (file-exists-p semanticdb-default-save-directory))
  (make-directory semanticdb-default-save-directory))

(add-hook 'after-init-hook 'global-company-mode)

; code browser
;(require 'ecb)
;(setq
; ecb-tree-buffer-style (quote ascii-guides)
; ecb-layout-name "left8"
; ecb-tip-of-the-day nil
; ecb-windows-width 30
; ecb-fix-window-size 'width
; ecb-auto-activate t)

; (defconst my-ecb-font
;           "DejaVu Sans Mono-6")
; (set-face-font 'ecb-default-general-face my-ecb-font)
; (set-face-font 'ecb-bucket-node-face my-ecb-font)

; syntax checking
(global-flycheck-mode)

(provide 'my-ide)
