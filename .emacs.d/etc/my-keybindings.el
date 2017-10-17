;;; my-keybindings.el -- Keyboard mapping

;;; Commentary:

;; bind-keys is used to set keybindings
;; Package-specific bindings are defined using use-package,
;; others are set here.

;;; Code:


;map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)


(bind-keys :prefix-map my-prefix
	   :prefix "M-q")
(bind-keys :map my-prefix
	   ("M-q" . fill-paragraph)
	   ("k" . describe-personal-keybindings)
	   ("l" . redraw-display))


;--------------------------------------- code-related prefix-c ---
(bind-keys :prefix-map my-prefix-code
	   :map my-prefix
	   :prefix "c"
	   ("c" . compile)
	   ("w" . global-whitespace-mode))


;---------------------------------------- git-related prefix-g ---
(bind-keys :prefix-map my-prefix-git
	   :map my-prefix
	   :prefix "g"
	   ("g" . magit-status)
	   ("RET" . magit-status)
	   ("b" . magit-blame)
           ("l" . magit-log-buffer-file)
	   ("s" . my-magit-show-branch))


;------------------------------------------- org-mode prefix-o ---
(bind-keys :prefix-map my-prefix-org
	   :map my-prefix
	   :prefix "o"
	   ("l" . org-store-link)
	   ("a" . org-agenda)
	   ("c" . org-capture)
	   ("b" . org-iswitchb))


;--------------------------------------- text-related prefix-t ---
(bind-keys :prefix-map my-prefix-text
	   :map my-prefix
	   :prefix "t"
	   ("f" . auto-fill-mode)
	   ("r" . rainbow-mode)
	   ("s" . flyspell-mode)
	   ("w" . delete-trailing-whitespace))


(provide 'my-keybindings)
;;; my-keybindings.el ends here
