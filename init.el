(setq inhibit-startup-message t)  ;; hide default emacs startup msg
(scroll-bar-mode -1)              ;; disable visual scrollbar
(tool-bar-mode -1)                ;; disable toolbar
(tooltip-mode -1)                 ;; tooltip is a overlay info, disable it 
(set-fringe-mode 10)              ;; The fringe is a thin strip on the left and/or right edge of a window. 
(menu-bar-mode -1)                ;; disable menu bar mode
(toggle-frame-fullscreen)         ;; start with fullscreen

;; download Fira code fonts which looks good. 
(set-face-attribute 'default nil :font "Fira Code" :height 150)



;; Customizing ModeLine with all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Make ESC key work like CRTL + G, or hide log/warning prompts 
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; =========== Packagae loading and sources ===================
(require 'package)

;; Deprioritize Melpa over all stable archives if same package is found on stable versions. 
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 )


      package-archive-priorities '(
				   ("melpa-stable". 10)
				   ("org". 9)
				   ("elpa". 8)
				   ("melpa". 1)
				   )
      )


(package-initialize)

(unless package-archive-contents (package-refresh-contents))


;; initialize use-package on non-linux distros
(unless (package-installed-p 'use-package) (package-install 'use-package) )

(require 'use-package)
(setq use-package-always-ensure t)

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

;; if all-icons-font is not installed prompt the user to install
;; manula installation via `M-x all-the-icons-install-fonts`
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))

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

;; Displays all associated key binding and their functions after set delay on the mini-mode
(use-package which-key
  :init(which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 2))

;; ORG-mode specifics
(use-package org
  :config
  (setq org-ellipsis " ▼")
  )

(defun bj/set-left-padding ()
     (set-window-margins (selected-window) 3 0)
  )

;; prettify bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  (org-mode . bj/set-left-padding)
  )

;; use org-tempo to allow faster code block addition
(require 'org-tempo)

(use-package org-superstar)

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

;; Zen Mode Writing
(use-package writeroom-mode)

;; magit setup
(use-package magit)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t))
 )

(defun org-babel-tangle-emacsconfig-on-save ()
    (when (string-equal (buffer-file-name)
			(expand-file-name "~/.emacs.d/EmacsConfig.org"))
      (let ((org-config-babel-evaluate nil))
	(org-babel-tangle))))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook #'org-babel-tangle-emacsconfig-on-save)))

;;; uses tab to show completions afte the thing is indented
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;; 	(url-retrieve-synchronously
;; 	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;; 	 'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (setq package-enable-at-startup nil)

(use-package org-roam-ui)

;; required for org-roam-export to be available
(require 'org-roam-export)
