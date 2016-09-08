; map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)

(windmove-default-keybindings) ; use S-arrows to move between windows

;(global-set-key "\t" 'company-complete-common)
(define-key global-map (kbd "RET") 'newline-and-indent)

;(global-set-key [M-left] 'tabbar-backward-tab)
;(global-set-key [M-right] 'tabbar-forward-tab)

;-------------------------------------------------- C-z prefix ---
(global-unset-key (kbd "M-q"))
(define-prefix-command 'my-prefix)
(global-set-key (kbd "M-q") 'my-prefix)
(global-set-key (kbd "M-q M-q") 'fill-paragraph)

;-------------------------------------- C-z prefix + single key---
(define-key my-prefix (kbd "w") 'ace-window)
(define-key my-prefix (kbd "M-w") 'ace-window)
(define-key my-prefix (kbd "l") 'redraw-display)

;--------------------------------------- code-related prefix-c ---
(define-key my-prefix (kbd "c c") 'compile)
(define-key my-prefix (kbd "c w") 'global-whitespace-mode)

;------------------------------------------- org-mode prefix-o ---
(define-key my-prefix (kbd "o l") 'org-store-link)
(define-key my-prefix (kbd "o a") 'org-agenda)
(define-key my-prefix (kbd "o c") 'org-capture)
(define-key my-prefix (kbd "o b") 'org-iswitchb)

(provide 'my-keybindings)
