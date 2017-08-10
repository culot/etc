;;; my-ui.el -- User interface configuration

;;; Commentary:

;; Here goes everything used to customize Emacs' appearance

;;; Code:


(bind-keys :prefix-map my-prefix-ui
	   :map my-prefix
	   :prefix "u")

(use-package dashboard
  :ensure t
  :config
  (setq
    dashboard-startup-banner 2
    dashboard-items '((recents . 20)
		      (bookmarks . 5)
		      (projects . 5)
		      (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package ace-window
  :ensure t
  :bind (:map my-prefix
	      ("w" . ace-window)
	      ("M-w" . ace-window))
  :config
    (setq aw-background nil))

(use-package eyebrowse
  :ensure t
  :demand t
  :bind (:map my-prefix-ui
	      ("k" . eyebrowse-close-window-config))
  :config
    (eyebrowse-mode)
    (eyebrowse-setup-opinionated-keys)
    (setq eyebrowse-new-workspace t))

(use-package smart-mode-line
  :ensure t
  :config
    (setq
      column-number-mode t
      sml/no-confirm-load-theme t
      sml/shorten-directory t
      sml/shorten-modes t
      sml/mule-info nil)
    (sml/setup))

(use-package neotree
  :ensure t
  :bind (:map my-prefix-ui
	      ("t" . neotree-toggle))
  :config
    (setq
       neo-smart-open t
       neo-theme 'nerd))
  
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(xterm-mouse-mode t)

(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 95 50))

(setq display-time-day-and-date t
      display-time-format "%d/%m %R"
      display-time-default-load-average nil)
(display-time-mode 1)

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

(load-theme 'melancholy t)

;(setq initial-scratch-message "ಠ_ಠ\n\n\n")

(provide 'my-ui)
;;; my-ui.el ends here
