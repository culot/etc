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

(provide 'my-ui)
