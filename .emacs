
;;; -*- emacs-lisp -*-

;; To edit this file from within Emacs interface, type:
;; <M-x> customize-group <ENTER> dotemacs <ENTER>
;;
;; History
;; 2008-08-01  my-exit-save-all function added (to use with mutt)
;; 2008-04-06  Complete rewrite of accumulated mess until now
;; 2014-11-26  Back to emacs and configuration of a few new modes (ido...)

;;;;;;;;;;;;;;;;;;; Internal functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DO NOT EDIT
(defun dotemacs-progress (text)
  "Display .emacs load progress if `dotemacs-show-progess' is true"
  (when (and (boundp 'dotemacs-show-progess)
	     dotemacs-show-progess)
    (let ((completetext (concat ".emacs : " text)))
      (message completetext)
      ;; avoid startup message
      (setq inhibit-startup-message t) ;; XEmacs
      (eval `(defun startup-echo-area-message () ,completetext)) ;; Emacs
)))

(defvar dotemacs-warns
  (list)
"List containing non-fatal errors encountered during .emacs load"
)

(defun require-optional (symbol &rest options)
"Similar to `require' but does not raise an error in case the required 
package is found to be missing. If the package is missing, `symbol' is 
inserted into the `dotemacs-warns' list, except if the `quiet' option
is also given.
"
(cond ((condition-case nil
	   (progn (require symbol) t)
	 (file-error nil)))
      ((member (symbol-name symbol) dotemacs-pkg-ignores)
       nil)
      ((not (memq 'quiet options))
       (add-to-list 'dotemacs-warns
		    (concat "Package \"" (symbol-name symbol) "\" missing")
		    'at-end-of-list)
       nil)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Configuration options")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If options get modified from within the interface, save changes in a
;; different file
(if (featurep 'xemacs)
    (setq custom-file "~/.xemacs-custom")
  (setq custom-file "~/.emacs-custom")
)
(if (file-exists-p custom-file) (load-file custom-file))

(defcustom dotemacs-show-progess t
"Display .emacs load messages"
:type '(boolean)
:group 'dotemacs
)

(defcustom dotemacs-pkg-ignores
  ()
"List of ignored packages"
:type '(repeat string)
:group 'dotemacs
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Site-specific configuration")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom load-path
  load-path ;; Defined hereafter
"Directory to store the Emacs-Lisp files
\"~/lib/emacs\" is used to store lisp files downloaded by the user.
\"~/lib/emacs/elc\" is used to store compiled files.
"
:type '(repeat file)
:group 'dotemacs
)
(setq load-path ( append
		  (list
		   (expand-file-name "~/lib/emacs/elc")
;                   (expand-file-name "~/lib/emacs/chess")
		   (expand-file-name "~/lib/emacs")
		   ) load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "My own functions")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-previous-window () 
  "Switch to previous window."
  (interactive)
  (other-window (- 1)))

(defun my-next-work-frame ()
  "Skip speedbar and other apps windows."
  (interactive)
  (let ((n (if (and (boundp 'speedbar-frame)
                    (eq (next-frame) speedbar-frame))
               2 1)))
      (other-frame n)))

(defun my-previous-work-frame ()
  "Skip speedbar and other apps windows."
  (interactive)
  (let ((n (if (and (boundp 'speedbar-frame)
                    (eq (previous-frame) speedbar-frame))
               -2 -1)))
      (other-frame n)))

(defun my-find-tag-noconfirm ()
  "Quickly find tag under cursor."
  (interactive)
  (find-tag (find-tag-default)))

(defun my-match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun my-fill-paragraph ()
  "fill and justify a paragraph.
A 4-spaces margin is applied, and a paragraph starts at 8 spaces."
  (interactive)
  (let ((fill-column 78)(left-margin 2))
    (fill-paragraph 1))
)

(defun my-compile ()
  "run make and move the point to the end of the output"
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (my-small-window)
      (command-execute 'compile)
      (read-string "Press [ENTER] to continue")
      (delete-window (get-buffer-window (get-buffer "*compilation*")))
      (select-window original-window)
    ))
)

(defun my-small-window ()
  "create a small window at bottom of screen"
  (interactive)
  (split-window-vertically -10)
)

(defun my-switch-to-header ()
  "Switch to a C/C++ header file.
When called in a buffer with example.c switches to file example.h from the
directory where example.c lives."
  (interactive)
  (let ((fname (buffer-file-name))
        hname)
    ;; If the current buffer is not visiting a file, signal an error
    (unless (stringp fname)
      (error "No file associated with the buffer %s" (buffer-name)))
    (if (string-match "\\(.\\.\\)[^.]" fname)
        ;; Replace extension with ".h"
        (setq hname (replace-match "\\1h" nil nil fname))
      ;; If no extension found then append ".h"
      (setq hname (concat fname ".h")))
    ;; Open corresponding header file
    (unless (file-readable-p hname)
      (error "No such file or file not readable, %s" hname))
   (find-file hname)))

(defun my-insert-date (prefix)
  "Insert the current date. Format depends on the number of prefixes:
   C-c d           -- insert-date: 2008-05-06
   C-u C-c d       -- insert-date: 06.05.2008
   C-u C-u C-c d   -- insert-date: Sunday, April 06, 2008"
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%d.%m.%Y")
                 ((equal prefix '(16)) "%A, %B %d, %Y"))))
    (insert (format-time-string format))))

(defun my-exit-save-all ()
  "Used to exit emacs, saving all files without prompting user.
   This is interesting while using emacs inside mutt to write mail,
   so that user doesn't need to confirm save before returning to mutt."
  (interactive)
  (save-buffers-kill-emacs t))

(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "KeyBindings")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys used in every mode
(global-set-key [f9] 'compile)
(global-set-key [f8] 'next-error)
(global-set-key "\M-p" 'my-previous-window)
(global-set-key "\M-n" 'other-window)
(global-set-key "\M-." 'my-find-tag-noconfirm)
(global-set-key "\C-\M-p" 'my-previous-work-frame)
(global-set-key "\C-\M-n" 'my-next-work-frame)
(global-set-key "\C-c)" 'match-paren)
(global-set-key "\C-cb" 'electric-buffer-list)
(global-set-key "\C-cc" 'my-compile)
(global-set-key "\C-c\C-c" 'my-exit-save-all)
(global-set-key "\C-cd" 'my-insert-date)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-ch" 'whitespace-mode)
(global-set-key "\C-cq" 'my-fill-paragraph)
(global-set-key "\C-ci" 'ispell)
(global-set-key "\C-cs" 'speedbar-get-focus)
(global-set-key "\C-cma" 'artist-mode)
(global-set-key "\C-cms" 'flyspell-mode)
(global-set-key "\C-cmf" 'follow-mode)
(global-set-key "\C-cmr" 'rst-mode)
(global-set-key "\C-cmh" 'hs-minor-mode)
(global-set-key "\C-c\C-fn" 'set-frame-name)
(global-set-key "\C-c\C-fs" 'select-frame-by-name)
(global-set-key "\C-ct" 'find-tag)
(global-set-key "\C-ck"
		(lambda ()
		  (interactive)
		  (let ((woman-topic-at-point t))
		    (woman))))

;; taken from:
;; http://www.tldp.org/HOWTO/Keyboard-and-Console-HOWTO-5.html
;; http://www.emacswiki.org/emacs/BackspaceKey
;;
;; Used in case meta-del is borked
;; to see which code is passed to emacs, press CTRL-Q and then the key
;; one can also check with M-x describe-key and then the key
;;
;(keyboard-translate ?\C-h ?\C-?)
;(keyboard-translate ?\C-? ?\C-h)

(global-set-key (kbd "C-M-h") 'backward-kill-word)
;(global-set-key (kbd "C-?") 'delete-backward-char)
;(global-set-key (kbd "M-?") 'mark-paragraph)
;(global-set-key (kbd "C-h") 'delete-backward-char)
;(global-set-key (kbd "M-h") 'backward-kill-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Look-and-feel")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)
(setq-default fill-column 80)
(show-paren-mode t)
(transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "File editing configuration")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(require-final-newline 1)
 '(next-line-add-newlines nil)
 '(indent-tabs-mode nil)
 '(default-major-mode 'text-mode)
)

;; Automatic text completion
(eval-after-load "dabbrev"
  '(defalias 'dabbrev-expand 'hippie-expand)) ;; find more completions
(custom-set-variables '(dabbrev-upcase-means-case-search t))

;; Auto update of modified file
(global-auto-revert-mode 1)

;; Read-Write of compressed files
(auto-compression-mode 1)

;; Automatic completion of file names inside mini-buffer
(setq minibuffer-confirm-incomplete t)

;; Do backups and put them into one single place
;; Backup files are those with '~' at the end
(setq make-backup-files t)
(let ((dir-bkp-files (expand-file-name "~/bak/emacs/")))
  (custom-set-variables `(backup-directory-alist
			  (list (cons "." ,dir-bkp-files)))))

;; Automatic saving of files
;; Saved files are those with '#' at the beginning and end
;; Also redefe the make-auto-save-file-name function in order to get
;; autosave files sent to a single directory.  Note that this function
;; looks first to determine if you have a ~/bak/emacs directory.  If
;; you do not it proceeds with the standard auto-save procedure.
(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.."
  (if buffer-file-name
      (if (file-exists-p "~/bak/emacs/") 
          (concat (expand-file-name "~/bak/emacs/") "#"
                  (replace-regexp-in-string "/" "!" buffer-file-name)
                  "#") 
         (concat
          (file-name-directory buffer-file-name)
          "#"
          (file-name-nondirectory buffer-file-name)
          "#"))
    (expand-file-name
     (concat "#%" (buffer-name) "#"))))
(setq 
 auto-save-default t
 auto-save-interval 10000
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Behavioral configuration")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bookmark-save-flag 1) ;; save each time you set a bookmark


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Interface configuration")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t ;; even if dotemacs-show-progess is nil
      visible-bell t
      truncate-partial-width-windows nil)
;(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
;(standard-display-european t)
(set-language-environment 'latin-1)
(set-terminal-coding-system 'latin-1)
(set-keyboard-coding-system 'latin-1)
(set-input-mode nil nil 1) ;; to properly display accents
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq scroll-margin 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Mode configuration")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotemacs-progress "Mode Ido") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)

(dotemacs-progress "Mode Windmove") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(windmove-default-keybindings 'meta)
;; The windmove bindings did not work for me in -nw (console) mode
;; However, I found a solution here:
;; https://github.com/gwtaylor/dotfiles/blob/master/.emacs.d/bindings.el
(define-key input-decode-map "\e\e[A" [(meta up)])
(define-key input-decode-map "\e\e[B" [(meta down)])
(define-key input-decode-map "\e\e[D" [(meta left)])
(define-key input-decode-map "\e\e[C" [(meta right)])
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOD" [(meta left)])
(define-key input-decode-map "\e\eOC" [(meta right)])

(dotemacs-progress "Mode CC") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq 
 compile-command "cd ../ && make"
 compilation-ask-about-save nil
 compilation-read-command nil
 compilation-scroll-output t
; compilation-window-height 10
)

(add-hook
  'c-mode-common-hook
  (lambda ()
    (c-set-style "gnu")
    (local-set-key (kbd "RET") 'newline-and-indent)
    (hs-minor-mode)
  )
)

(dotemacs-progress "Mode PO") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

(dotemacs-progress "Mode Perl") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'perl-mode 'cperl-mode)
(setq 
 cperl-lazy-help-time 2
 cperl-set-style 'GNU
)

(dotemacs-progress "Mode Python") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require-optional 'python-mode)
  (autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
)

(dotemacs-progress "Mode ReST") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require-optional 'rst)
  (add-hook 'rst-mode-hook 'rst-text-mode-bindings)
  (setq auto-mode-alist
	(append '(("\\.rst$" . rst-mode)
		  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (setq font-lock-global-modes '(not rst-mode))
)

(dotemacs-progress "Mode VC") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable local copies like "foo.bar.~1.1~ ..."
(eval-after-load "vc-cvs" '(defun vc-cvs-make-version-backups-p (ignored) nil))

(dotemacs-progress "Mode saveplace") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(setq-default save-place t)

(dotemacs-progress "Mode Spelling") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default ispell-program-name "aspell")
;(setq ispell-dictionary "francais")
;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling." t)

(dotemacs-progress "Mode Speedbar") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require-optional 'speedbar)
  (setq speedbar-change-initial-expansion-list "buffers")
)

(dotemacs-progress "Mode Woman") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; view manpage about word under point without prompting
; under OpenBSD, manpage can be stored into cat?
(setq woman-manpath-man-regexp "\\(cat\\)\\|\\(man\\)")

(dotemacs-progress "Mode Font-Lock") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require-optional 'font-lock)
  (cond ((fboundp 'global-font-lock-mode)
	 ;; Customize face attributes
	 (setq font-lock-face-attributes
	       ;; Symbol-for-Face Foreground Background Bold Italic Underline
	       '((font-lock-comment-face       "Purple" )
;               (font-lock-string-face        "DarkGreen" )
;               (font-lock-keyword-face       "Cyan" )
;               (font-lock-function-name-face "LightBlue" )
;               (font-lock-variable-name-face "Purple" )
;               (font-lock-type-face          "Cyan" )
;               (font-lock-constant-face      "Purple" )
		 ))
	 ;; Maximum colors
	 (setq font-lock-maximum-decoration t)))
)

(dotemacs-progress "Mode vvb") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require-optional 'vvb-mode)
  (setq-default vvb-column 80
                vvb-right-on-eol-p nil
                vvb-sticky-p nil
                vvb-permanent-p t) 
  (add-hook 'c-mode-common-hook 'vvb-mode)
)

;(dotemacs-progress "Mode Chess") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(autoload 'chess "chess" "Play a game of chess" t)
;(setq chess-default-engine 'chess-gnuchess)
;(setq chess-default-display 'chess-plain)

(dotemacs-progress "Other modes") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on automatic line break for and only for ASCII editing
(setq-default auto-fill-function 'do-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (dotemacs-progress "Goodies")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook
	  (lambda () (dotemacs-progress
		      (cond (dotemacs-warns
			     "Loading successful but with warnings, type C-h v dotemacs-warns")
			    (t "Loading successful ;)")))))
