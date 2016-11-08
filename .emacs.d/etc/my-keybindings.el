; map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)

(windmove-default-keybindings) ; use S-arrows to move between windows

;(global-set-key "\t" 'company-complete-common)
;(define-key global-map (kbd "RET") 'newline-and-indent)

;(global-set-key [M-left] 'tabbar-backward-tab)
;(global-set-key [M-right] 'tabbar-forward-tab)

;-------------------------------------------------- M-q prefix ---
(global-unset-key (kbd "M-q"))
(define-prefix-command 'my-prefix)
(global-set-key (kbd "M-q") 'my-prefix)
(global-set-key (kbd "M-q M-q") 'fill-paragraph)

;-------------------------------------- M-q prefix + single key---
(define-key my-prefix (kbd "w") 'ace-window)
(define-key my-prefix (kbd "M-w") 'ace-window)
(define-key my-prefix (kbd "l") 'redraw-display)

;--------------------------------------- code-related prefix-c ---
(define-key my-prefix (kbd "c c") 'compile)
(define-key my-prefix (kbd "c w") 'global-whitespace-mode)

;---------------------------------------- git-related prefix-g ---
(define-key my-prefix (kbd "g RET") 'magit-status)

;------------------------------------------- org-mode prefix-o ---
(define-key my-prefix (kbd "o l") 'org-store-link)
(define-key my-prefix (kbd "o a") 'org-agenda)
(define-key my-prefix (kbd "o c") 'org-capture)
(define-key my-prefix (kbd "o b") 'org-iswitchb)

;--------------------------------------- text-related prefix-t ---
(define-key my-prefix (kbd "t f") 'auto-fill-mode)
(define-key my-prefix (kbd "t s") 'flyspell-mode)

(provide 'my-keybindings)
