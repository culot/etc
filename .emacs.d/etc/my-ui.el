(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-8"))

(load-theme 'solarized-light t)

(require 'powerline)
(powerline-default-theme)

(display-time-mode 1)

;(require 'sr-speedbar)
;(setq
;   speedbar-use-images nil
;   sr-speedbar-right-side t
;   sr-speedbar-width 20
;   sr-speedbar-width-console 20
;   sr-speedbar-max-width 20
;   sr-speedbar-auto-refresh t
;   speedbar-initial-expansion-list-name "buffers")
;(make-face 'speedbar-face)
;(set-face-font 'speedbar-face "DejaVu Sans Mono-6")
;(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

(provide 'my-ui)
