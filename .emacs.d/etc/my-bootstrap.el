; Check that packages are all there and install them if needed

(require 'package)
(setq package-user-dir my-dir-opt)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

; Automatically install the packages I need:
(setq package-list '(
  ; packages
  async              ; make it faster
  powerline          ; better status bar
  spaceline          ; add-ons for powerline
;  sr-speedbar        ; same-frame speedbar
  yasnippet          ; yet another snippet extension
  flycheck           ; on-the-fly syntax checking
  company            ; auto completion
  helm               ; completion and selection framework
;  workgroups2        ; save sessions
  tabbar             ; easily switch between buffers
  symon              ; system monitor
  
  ; ide
  projectile         ; project interaction library
  helm-projectile    ; integrate projectile with helm
  ecb                ; emacs code browser
  
  ; color themes:
;  solarized-theme
;  mustang-theme
;  zenburn-theme
;  ubuntu-theme
  moe-theme
))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'my-bootstrap)
