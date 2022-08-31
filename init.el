(setq inhibit-startup-message t)  ;; hide default emacs startup msg
(scroll-bar-mode -1)              ;; disable visual scrollbar
(tool-bar-mode -1)                ;; disable toolbar
(tooltip-mode -1)                 ;; tooltip is a overlay info, disable it 
(set-fringe-mode 10)              ;; The fringe is a thin strip on the left and/or right edge of a window. 
(menu-bar-mode -1)                ;; disable menu bar mode
(toggle-frame-fullscreen)         ;; start with fullscreen 

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


;; Dark theme. Non-dark variant is found with spacemacs-light
;; alternative good option is tango-dark.
;; unsing t at the end supresses interactive prompt.
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))


;; Customizing ModeLine
(use-package all-the-icons
  :if (display-graphic-p))

;; if all-icons-font is not installed prompt the user to install
;; manula installation via `M-x all-the-icons-install-fonts`
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))


;; magit
(use-package magit)


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
  (selectrum-mode 1)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(emacsql-sqlite3 org-roam writeroom-mode org-superstar marginalia consult selectrum-prescient which-key use-package spacemacs-theme magit doom-modeline counsel all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; This is required for org-roam to not error on database connection. 
(use-package emacsql-sqlite3)
(setq org-roam-database-connector 'sqlite3)

;; install org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roamnotes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  )


;; magit
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


