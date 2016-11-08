; Check that packages are all there and install them if needed

; Note: in case this list changes, it might be necessary to use:
;   M-x package-refresh-contents RET

(require 'package)
(setq package-user-dir my-dir-opt)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

; Automatically install the packages I need:
(setq package-list '(
  ; packages
  async              ; make it faster
  smart-mode-line    ; better status bar
;  sr-speedbar        ; same-frame speedbar
;  yasnippet          ; yet another snippet extension
  flycheck           ; on-the-fly syntax checking
  company            ; auto completion
  helm               ; completion and selection framework
  eyebrowse          ; manage windows ala i3wm
;  workgroups2        ; save sessions
;  tabbar             ; easily switch between buffers
  symon              ; system monitor
  ace-window         ; visual way to switch between windows

  ; ide
  projectile         ; project interaction library
  ggtags             ; indexer used by projectile
  magit              ; interface to Git
;  helm-projectile    ; integrate projectile with helm
;  ecb                ; emacs code browser

  ; color themes:
  rainbow-mode
  solarized-theme
  color-theme-sanityinc-tomorrow
;  mustang-theme
;  zenburn-theme
;  ubuntu-theme
;  moe-theme
  ample-theme
))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'my-bootstrap)
