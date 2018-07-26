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
	   :prefix "c"
	   :map my-prefix
	   ("c" . compile)
	   ("w" . global-whitespace-mode))


;---------------------------------------- git-related prefix-g ---
(bind-keys :prefix-map my-prefix-git
	   :prefix "g"
	   :map my-prefix
	   ("g" . magit-status)
	   ("RET" . magit-status)
	   ("b" . magit-blame)
           ("l" . magit-log-buffer-file)
	   ("s" . my-magit-show-branch))


;------------------------------------------- org-mode prefix-o ---
(bind-keys :prefix-map my-prefix-org
	   :prefix "o"
	   :map my-prefix
	   ("l" . org-store-link)
	   ("a" . org-agenda)
	   ("c" . org-capture)
	   ("b" . org-iswitchb))


;--------------------------------------- text-related prefix-t ---
(bind-keys :prefix-map my-prefix-text
	   :prefix "t"
	   :map my-prefix
	   ("f" . auto-fill-mode)
	   ("r" . rainbow-mode)
	   ("s" . flyspell-mode)
	   ("w" . delete-trailing-whitespace))


(provide 'my-keybindings)
;;; my-keybindings.el ends here
