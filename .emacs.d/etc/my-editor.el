;; Options

(global-font-lock-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(setq auto-save-list-file-prefix my-dir-bak)
(setq auto-save-file-name-transforms `((".*",my-dir-bak t)))
(setq backup-directory-alist (list (cons ".*" my-dir-bak)))


(when (locate-library "recentf")
  (setq recentf-save-file (expand-file-name "recentf" my-dir-var)
        recentf-max-saved-items 100
        recentf-max-menu-items  50
        recentf-exclude '("/Gnus/" "\\`/[a-zA-Z0-9@]+:"))
    (require 'recentf)
    (recentf-mode 1))

(provide 'my-editor)
