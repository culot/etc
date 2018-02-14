;;; my-go.el -- golang configuration

;;; Commentary:

;; 2018-02-14 - Initial version

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq gofmt-command "goimports")
	      (setq tab-width 4)
	      (setq indent-tabs-mode 1))))

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
