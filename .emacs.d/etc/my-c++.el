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

(c-add-style "my-c++-style"
	     '("stroustrup"
               (c-syntactic-indentation . 1)   ; automatic indentation
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 2)))          ; indent by two spaces

(defun my-c++-mode-hook ()
  (c-set-style "my-c++-style")
  (auto-fill-mode)
  (ggtags-mode 1)
  (c-auto-newline nil)          ; disable newline when hitting {};
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'my-c++)
;;; my-c++.el ends here
