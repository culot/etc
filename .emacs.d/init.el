; Set directory structure and create it if necessary

(defvar my-dir-root (expand-file-name "~/.emacs.d/"))
(defvar my-dir-etc (concat my-dir-root "etc/"))
(defvar my-dir-var (concat my-dir-root "var/"))
(defvar my-dir-opt (concat my-dir-var "opt/"))
(defvar my-dir-bak (concat my-dir-var "bak/"))
(defvar my-dir-eshell (concat my-dir-var "eshell/"))

(unless (file-exists-p my-dir-var)
  (make-directory my-dir-var t))

(unless (file-exists-p my-dir-opt)
  (make-directory my-dir-opt t))

(unless (file-exists-p my-dir-bak)
  (make-directory my-dir-bak t))

; Then load configuration
(add-to-list 'load-path my-dir-etc)

; Early requires
(require 'my-bootstrap)

; Standard requires
(require 'my-editor)
(require 'my-ui)
(require 'my-eshell)

; Late requires
(require 'my-keybindings)
