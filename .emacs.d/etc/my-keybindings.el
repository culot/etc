; map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)

(windmove-default-keybindings) ; use S-arrows to move between windows
; also define prefixed arrow keys for better terminal support
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key "\t" 'company-complete-common)
(define-key global-map (kbd "RET") 'newline-and-indent)

;(global-set-key [M-left] 'tabbar-backward-tab)
;(global-set-key [M-right] 'tabbar-forward-tab)

;; Use C-z as prefix key
(global-unset-key (kbd "C-z"))
(define-prefix-command 'my-prefix)
(global-set-key (kbd "C-z") 'my-prefix)
(global-set-key (kbd "C-z C-z") 'suspend-frame)

; code-related
(define-key my-prefix (kbd "c c") 'compile)
(define-key my-prefix (kbd "c w") 'global-whitespace-mode)

(provide 'my-keybindings)
