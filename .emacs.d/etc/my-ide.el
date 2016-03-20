; quickly access a project's files
(projectile-global-mode)
(setq projectile-enable-caching t)

; enable completion
(semantic-mode 1)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

; code browser
(require 'ecb)
(setq
 ecb-tree-buffer-style (quote ascii-guides)
 ecb-layout-name "left8"
 ecb-tip-of-the-day nil
 ecb-windows-width 30
 ecb-fix-window-size 'width)
(defconst my-ecb-font
          "DejaVu Sans Mono-6")
(set-face-font 'ecb-default-general-face my-ecb-font)
(set-face-font 'ecb-bucket-node-face my-ecb-font)

(provide 'my-ide)
