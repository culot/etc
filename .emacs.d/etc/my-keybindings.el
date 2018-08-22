;;; my-keybindings.el -- Keyboard mapping

;;; Commentary:

;; hydra is used to set keybindings

;;; Code:


;map backspace key for terminal
(keyboard-translate ?\C-h ?\C-?)


(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))


(defhydra hydra-main (:color blue
		      :hint nil)
  "
^Actions^             ^Help
^^^^^^^^-----------------------------------------------------------------
  _M-q_ fill para.    _k_ keybindings
    _l_ redraw        ^ ^
_w_ _M-w_ ace-window  ^ ^

"

  ; Most used (and thus accessible) commands
  ("M-q" fill-paragraph)
  ("k" describe-personal-keybindings)
  ("l" redraw-display)
  ("w" ace-window)
  ("M-w" ace-window)

  ; nested hydras
  ("c" hydra-code/body "code" :exit t)
  ("g" hydra-git/body "git" :exit t)
  ("o" hydra-org/body "org" :exit t)
  ("t" hydra-text/body "text" :exit t)
  ("u" hydra-ui/body "ui" :exit t)
)


(defhydra hydra-code (:color blue
		      :hint nil)
  "
^Actions^             ^Toggles
^^^^^^^^-----------------------------------------------------------------
_c_ compile           _w_ whitespace-mode: %`global-whitespace-mode

"
  ("c" compile)
  ("w" global-whitespace-mode))

(defhydra hydra-git (:color blue
		     :hint nil)
  "
^Actions
^^^^^^^^-----------------------------------------------------------------
_g_ _RET_ status
    _b_ blame
    _l_ log
    _s_ show branch
"
  ("g" magit-status)
  ("RET" magit-status)
  ("b" magit-blame)
  ("l" magit-log-buffer-file)
  ("s" my-magit-show-branch))

(defhydra hydra-org (:color blue
		     :hint nil)
  "
^Actions
^^^^^^^^-----------------------------------------------------------------
_l_ link
_a_ agenda
_c_ capture
_b_ switch buffer
"
  ("l" org-store-link)
  ("a" org-agenda)
  ("c" org-capture)
  ("b" org-iswitchb))

(defhydra hydra-text (:color blue
		      :hint nil)
  "
^Actions^             ^Modes
^^^^^^^^-----------------------------------------------------------------
_w_ del trail spaces  _f_ auto-fill
^ ^                   _r_ rainbow
^ ^                   _s_ flyspell
"
  ("f" auto-fill-mode)
  ("r" rainbow-mode)
  ("s" flyspell-mode)
  ("w" delete-trailing-whitespace))

(defhydra hydra-ui (:color blue
		    :hint nil)
  "
^Actions
^^^^^^^^-----------------------------------------------------------------
_h_ highlight line
_k_ close window
_b_ beacon
_t_ neotree
"
  ("h" global-hl-line-mode)
  ("k" eyebrowse-close-window-config)
  ("b" beacon-blink)
  ("t" neotree-toggle))


(global-set-key (kbd "M-q") 'hydra-main/body)


(provide 'my-keybindings)
;;; my-keybindings.el ends here
