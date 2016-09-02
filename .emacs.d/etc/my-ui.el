(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)

(xterm-mouse-mode t)

(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 95 50))

(require 'powerline)
;(powerline-default-theme)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(require 'switch-window)

; Manage windows ala i3wm
(eyebrowse-mode t)
; Use optional keybindings to switch between workspaces with M-1..9
(eyebrowse-setup-opinionated-keys)
; Create new workspaces with scratch buffer (do not replicate previous one)
(setq eyebrowse-new-workspace t)

;(require 'tabbar)
;(setq tabbar-use-images nil)
;(tabbar-mode)

;(require 'symon)
;(symon-mode)

;; solarized theme and configuration

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Use more italics
;(setq solarized-use-more-italic t)

;(load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

;; moe theme and configuration
;(require 'moe-theme)
; automatically change between light/dark depending on time of day
;(require 'moe-theme-switcher)
;(powerline-moe-theme)
;(setq moe-theme-highlight-buffer-id t)
;(moe-theme-set-color 'purple)

(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

;(require 'sr-speedbar)
;(setq
;   speedbar-use-images nil
;   sr-speedbar-right-side t
;   sr-speedbar-width 20
;   sr-speedbar-width-console 20
;   sr-speedbar-max-width 20
;   sr-speedbar-auto-refresh t
;   speedbar-initial-expansion-list-name "buffers")
;(make-face 'speedbar-face)
;(set-face-font 'speedbar-face "DejaVu Sans Mono-6")
;(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

; set fonts after the theme was loaded
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-8"))

(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive)))
     (mapc
      (lambda (face) (set-face-attribute face nil :font "DejaVu Sans Mono-8"))
      faces))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'my-ui)
