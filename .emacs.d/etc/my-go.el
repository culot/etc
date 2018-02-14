;;; my-go.el -- golang configuration

;;; Commentary:

;; 2018-02-14 - Initial version

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

; Shows type information for variables, functions,
; and current argument position of function
(use-package go-eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'my-go)
;;; my-go.el ends here
