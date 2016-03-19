; Check that packages are all there and install them if needed

(require 'package)
(setq package-user-dir my-dir-opt)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

; Automatically install the packages I need:
(setq package-list '(
  ; packages
  async              ; make it faster
  powerline          ; better status bar
  
  ; color themes:
  solarized-theme
))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'my-bootstrap)
