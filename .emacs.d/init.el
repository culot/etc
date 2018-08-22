;;; init.el -- Main configuration file

;;; Commentary:

;;; Code:

;; Set directory structure and create it if necessary
(defvar my-dir-root (expand-file-name "~/.emacs.d/"))
(defvar my-dir-etc (concat my-dir-root "etc/"))
(defvar my-dir-lib (concat my-dir-root "lib/"))
(defvar my-dir-var (concat my-dir-root "var/"))
(defvar my-dir-opt (concat my-dir-root "opt/"))
(defvar my-dir-bak (concat my-dir-var "bak/"))
(defvar my-dir-eshell (concat my-dir-var "eshell/"))
(defvar my-dir-semantic (concat my-dir-var "semantic/"))
(defvar my-dir-projectile (concat my-dir-var "projectile/"))

(unless (file-exists-p my-dir-var)
  (make-directory my-dir-var t))
(unless (file-exists-p my-dir-opt)
  (make-directory my-dir-opt t))
(unless (file-exists-p my-dir-bak)
  (make-directory my-dir-bak t))


;; Iinitialize package framework
(require 'package)
(setq package-enable-at-startup nil)
(setq package-user-dir my-dir-opt)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Load personnal configuration
(add-to-list 'load-path my-dir-etc)

(require 'my-keybindings)
(require 'my-editor)
(require 'my-ui)
(require 'my-eshell)
(require 'my-ide)
(require 'my-c++)
(require 'my-go)
(require 'my-custom)

(provide 'init)
;;; init.el ends here
