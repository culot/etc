;;; my-ui.el -- User interface configuration

;;; Commentary:

;; Here goes everything used to customize Emacs' appearance

;;; Code:


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

;(use-package zone
;  :config
;    (zone-when-idle 120))

(use-package hl-line
  :init (global-hl-line-mode 1)
  :diminish global-hl-line-mode)

(use-package ace-window
  :ensure t
  :config
    (setq aw-background nil))

(use-package eyebrowse
  :ensure t
  :demand t
  :config
    (eyebrowse-mode)
    (eyebrowse-setup-opinionated-keys)
    (setq eyebrowse-new-workspace t))

(use-package telephone-line
  :config
    (use-package telephone-line-utils)
    (setq telephone-line-primary-left-separator 'telephone-line-flat)
    (setq telephone-line-primary-right-separator 'telephone-line-flat)

  ;; Exclude some buffers in modeline
  (defvar modeline-ignored-modes nil
    "List of major modes to ignore in modeline")

  (setq modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell"
				 "Term"
                                 "Debugger"
                                 "Quickrun"
                                 "REPL"
                                 "IELM"
                                 "Messages"))

  ;; Display modified status
  (telephone-line-defsegment my-telephone-line-modified-status-segment ()
    (when (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)))
      (propertize "+" 'face `(:foreground "red"))))

    (telephone-line-defsegment* my-telephone-line-buffer-segment ()
      `(""
        ,(telephone-line-raw mode-line-buffer-identification t)))

    (telephone-line-defsegment* my-telephone-line-position-segment ()
      (if (telephone-line-selected-window-active)
	  (when (not (member mode-name modeline-ignored-modes))
	    (if (eq major-mode 'paradox-menu-mode)
		(telephone-line-trim (format-mode-line mode-line-front-space))
	      '(" %3l,%2c ")))))

    (telephone-line-defsegment* my-telephone-line-vc-segment ()
      (if (telephone-line-selected-window-active)
	  (telephone-line-raw vc-mode t)))

    (telephone-line-defsegment* my-telephone-line-major-mode-segment ()
      (if (telephone-line-selected-window-active)
	  " /%[%m%]/"))

    (telephone-line-defsegment* my-telephone-line-minor-mode-segment ()
      (if (telephone-line-selected-window-active)
	  (telephone-line-raw minor-mode-alist t)))

    (telephone-line-defsegment* my-telephone-line-misc-segment ()
      (if (telephone-line-selected-window-active)
	  (eyebrowse-mode-line-indicator)))

    (setq telephone-line-lhs
	  '((nil . (my-telephone-line-buffer-segment))
	    (nil . (my-telephone-line-modified-status-segment))))
    (setq telephone-line-rhs
	  '((nil . (my-telephone-line-position-segment))
	    (accent . (my-telephone-line-vc-segment))
	    (nil . (my-telephone-line-minor-mode-segment))
	    (nil . (my-telephone-line-major-mode-segment))
	    (nil . (my-telephone-line-misc-segment))))
    (telephone-line-mode 1))

(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode
  :config
    (setq beacon-blink-delay 0.3
	  beacon-blink-duration 0.3
	  beacon-blink-when-point-moves-vertically 3
	  beacon-size 40))

(use-package neotree
  :ensure t
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

(add-to-list 'custom-theme-load-path "~/.emacs.d/etc")
(load-theme 'melancholy t)

;(setq initial-scratch-message "ಠ_ಠ\n\n\n")

(provide 'my-ui)
;;; my-ui.el ends here
