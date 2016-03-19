;; Options

(global-font-lock-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(setq auto-save-list-file-prefix my-dir-bak)
(setq auto-save-file-name-transforms `((".*",my-dir-bak t)))
(setq backup-directory-alist (list (cons ".*" my-dir-bak)))

(provide 'my-editor)
