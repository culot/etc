;;; my-c++.el -- C/C++ code configuration

;;; Commentary:

;; I code in C/C++ most of the time, so I created a sepcific configuration
;; file to store my preferences for those languages

;;; Code:

(when (executable-find "global")
  (use-package ggtags
    :defer t
    :bind (:map my-prefix-code
		("f" . ggtags-find-tag-regexp))))

(use-package google-c-style
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(defun my-c++-mode-hook ()
  (ggtags-mode 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'my-c++)
;;; my-c++.el ends here
