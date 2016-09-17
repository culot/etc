(custom-set-variables
 '(ecb-options-version "2.40")
 '(eyebrowse-mode-line-separator "|")
)


(custom-set-faces
 '(mode-line-inactive ((t :foreground "brightcyan" :background "black" :inverse-video nil)))
 '(mode-line     ((t :foreground "brightcyan" :background "blue" :inverse-video nil)))
 '(sml/global ((t (:foreground "brightcyan" :inverse-video nil))))
 '(sml/prefix ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/filename ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/line-number ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/col-number ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/git ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/modes ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/minor-modes ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/time ((t :inherit sml/global :foreground "brightcyan" :inverse-video nil)))
 '(sml/modified ((t :inherit sml/global :foreground "red" :inverse-video nil)))
 '(which-func ((t (:foreground "brightcyan"))))
 '(eyebrowse-mode-line-active ((t (:foreground "red"))))
)

(setq org-agenda-files '("~/org"))

(setq bookmark-save-flag 1) ; save bookmark file whenever it changes
(setq bookmark-default-file (concat my-dir-var "bookmarks"))

(setq initial-scratch-message "ಠ_ಠ\n\n\n")

(provide 'my-custom)
