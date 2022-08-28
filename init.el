(setq inhibit-startup-message t)  ;; hide default emacs startup msg
(scroll-bar-mode -1)              ;; disable visual scrollbar
(tool-bar-mode -1)                ;; disable toolbar
(tooltip-mode -1)                 ;; tooltip is a overlay info, disable it 
(set-fringe-mode 10)              ;; The fringe is a thin strip on the left and/or right edge of a window. 
(menu-bar-mode -1)                ;; disable menu bar mode


;; download Fira code fonts which looks good. 
(set-face-attribute 'default nil :font "Fira Code" :height 150)


;; use M-x eval-buffer to run this.
;; use M-x describe-function to get help


;; Make ESC key work like CRTL + G, or hide log/warning prompts 
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; =========== Packagae loading and sources ===================
(require 'package)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))


;; initialize use-package on non-linux distros
(unless (package-installed-p 'use-package) (package-install 'use-package) )

(require 'use-package)
(setq use-package-always-ensure t)

;; package to show command being used and what the function it calls
; (use-package command-log-mode)
;; didn't work


;; line and column numbers show
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numebrs for specific modes
(dolist (mode '(
		; list of modes 
		org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
	 (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package spacemacs-theme)

;; Dark theme. Non-dark variant is found with spacemacs-light
;; alternative good option is tango-dark. 
(load-theme 'spacemacs-dark)


;; Customizing ModeLine
(use-package all-the-icons
  :if (display-graphic-p))

;; if all-icons-font is not installed prompt the user to install
;; manula installation via `M-x all-the-icons-install-fonts`
;; TODO:

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))


;; magit
(use-package magit)



;;=================== disable warnings from emacs 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(which-key magit doom-modeline counsel spacemacs-theme use-package '(warning-suppress-log-types '((use-package) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Displays all associated key binding and their functions after set delay on the mini-mode
(use-package which-key
  :init(which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 2))

;; fuzzy search on M-x for use with Selectrum
;; for example: `M-x swi buf` to get to Switch Buffer
(use-package selectrum-prescient)

;; Simple and fast M-x completion mode
(use-package selectrum
  :functions 'selectrum-mode
  :init
  (selectrum-prescient-mode +1))

;; Provides additonal consulting mode for M-x
;; use `consult-` keys on M-x
(use-package consult)

;; Enable richer annotations using the Marginalia package
;; For example; shows description on right hand side.
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))



;; ORG-mode specifics
(use-package org)
(use-package org-superstar)


;; Writing specifics
(use-package writeroom-mode)
