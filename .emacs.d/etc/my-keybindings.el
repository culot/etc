(windmove-default-keybindings) ; use S-arrows to move between windows

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
