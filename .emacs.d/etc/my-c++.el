(c-add-style "my-c++-style" 
	     '("stroustrup"
               (c-syntactic-indentation . 1)   ; automatic indentation
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 2)))          ; indent by two spaces

(defun my-c++-mode-hook ()
  (c-set-style "my-c++-style")
  (auto-fill-mode)
  (c-auto-newline nil)          ; disable newline when hitting {};
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'my-c++)
