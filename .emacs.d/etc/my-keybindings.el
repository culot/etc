; map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)

(windmove-default-keybindings) ; use S-arrows to move between windows
; also define prefixed arrow keys for better terminal support
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key "\t" 'company-complete-common)

;(global-set-key [M-left] 'tabbar-backward-tab)
;(global-set-key [M-right] 'tabbar-forward-tab)

(global-unset-key (kbd "C-0"))
(define-prefix-command 'my-prefix)
(global-set-key (kbd "C-0") 'my-prefix)

; code-related
(define-key my-prefix (kbd "c c") 'compile)
(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'my-keybindings)
