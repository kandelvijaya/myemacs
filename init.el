(setq inhibit-startup-message t)  ;; hide default emacs startup msg
(scroll-bar-mode -1)              ;; disable visual scrollbar
(tool-bar-mode -1)                ;; disable toolbar
(tooltip-mode -1)                 ;; tooltip is a overlay info, disable it 
(set-fringe-mode 10)              ;; The fringe is a thin strip on the left and/or right edge of a window. 
(menu-bar-mode -1)                ;; disable menu bar mode
(toggle-frame-fullscreen)         ;; start with fullscreen

;; Y/N is better
(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message "; Welcome Back \n")

;; download Fira code fonts which looks good. 
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

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

;; if all-icons-font is not installed prompt the user to install
;; manula installation via `M-x all-the-icons-install-fonts`
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))

(setq debug-on-error t)

;;; uses tab to show completions
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;;  (setq recentf-mode t)
;;  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/.recentf"))

;; fuzzy search on M-x for use with Selectrum
;; for example: `M-x swi buf` to get to Switch Buffer
(use-package selectrum-prescient)

;; Simple and fast M-x completion mode
 (use-package selectrum
  :functions 'selectrum-mode
  :init
  (selectrum-mode 1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (use-package vertico
 ;;   :init
 ;;   (vertico-mode)
 ;;   )

 ;; ;; Optionally use the orderless completion style.
 ;; (use-package orderless
 ;;   :init
 ;;   ;; Configure a custom style dispatcher (see the Consult wiki)
 ;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
 ;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
 ;;   (setq completion-styles '(initials orderless )
 ;; 	completion-category-defaults nil
 ;; 	completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; ;; Provides additonal consulting mode for M-x
;; use `consult-` keys on M-x
(use-package consult
  :bind (
	 ;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x b" . consult-buffer)
	 ("M-s d" . consult-find)
	 ("M-s l" . consult-line)
	 ;; C-s is mapped to consult line 
	 ("C-s" . consult-line)
	 ))

;; Enable richer annotations using the Marginalia package
 ;; For example; shows description on right hand side.
 (use-package marginalia
   ;; The :init configuration is always executed (Not lazy!)
   :init

   ;; Must be in the :init section of use-package such that the mode gets
   ;; enabled right away. Note that this forces loading the package.
   (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Displays all associated key binding and their functions after set delay on the mini-mode
(use-package which-key
  :init(which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 2))

(defun bj-helpful-callable-in-another-window ()
  "Helpful-callable create a new window and when pressing ESC it actually kills the initiating window.

   This function switches the window once helpful-callabel is done so at to make ESC work as expected
  "
  (interactive)
  (progn 
    (call-interactively #'helpful-callable)
    (call-interactively #'other-window)
  )
)



(use-package helpful)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'bj-helpful-callable-in-another-window)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ORG-mode specifics
(use-package org
  :config
  (setq org-ellipsis " ▼")
  )

;; use org-tempo to allow faster code block addition
(require 'org-tempo)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-ellipsis " ▼")
  (setq org-superstar-headline-bullets-list
	'("◉" "◑" "◐" "◷" "▷")))

(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 10)
  (setq right-margin-width 0))

;; Add margins by default to a mode
(add-hook 'org-mode-hook 'my-set-margins)

;; This is required for org-roam to not error on database connection. 
(use-package emacsql-sqlite3)
(setq org-roam-database-connector 'sqlite3)

;; install org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Library/Mobile Documents/com~apple~CloudDocs/All Notes/roamnotes/"))
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

(use-package projectile
 :diminish projectile-mode
:ensure t
:pin melpa-stable
:init
(progn
 (projectile-mode +1)
 (when (file-directory-p "~/Projects/")
  (setq projectile-project-search-path '("~/Projects/")))
 (when (file-directory-p "~/Work/")
   (push "~/Work/" projectile-project-search-path)
   )
)
:bind (:map projectile-mode-map
            ("C-c p" . projectile-command-map)))

(use-package consult-projectile)

(require 'ob-shell)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (restclient . t)
   (shell . t)
   (ruby . t)
   ;; (swift . t) there is no ob-swift TODO
   (python . t))
 )

(defun org-babel-tangle-emacsconfig-on-save ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.emacs.d/EmacsConfig.org"))
      (let ((org-config-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'org-babel-tangle-emacsconfig-on-save)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         ;;(swift-mode . lsp-deferred)
         )
  :commands (lsp lsp-deffered))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp-deferred))))  ; or lsp-deferred
(use-package lsp-ui)



(defun my/autoparens () (electric-pair-mode t))

(add-hook 'emacs-lisp-mode-hook #'my/autoparens)
(add-hook 'lisp-interaction-mode-hook #'my/autoparens)

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

(setq org-agenda-files (directory-files-recursively "~/Library/Mobile Documents/com~apple~CloudDocs/All Notes/roamnotes/" "\\.org$"))

;; required for org-roam-export to be available
(require 'org-roam-export)

(setq org-startup-folded 'show2levels)

(setq org-src-window-setup 'split-window-right)

(defun bj-indent-org-mode ()
  (org-indent-mode t))

(add-hook 'org-mode-hook #'bj-indent-org-mode)

(use-package restclient)

;; install org-babel hook
(use-package ob-restclient)

;; enables Org babel lang integration
(require 'ob-ruby)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))



(defun bj-search-web-at-point ()
  (interactive)
  (eshell-command (concat "open -a firefox " "https://www.google.com/search?q=" (word-at-point)))
  )

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; install nano-theme
;; (use-package nano-theme
;;    :init (load-theme 'nano-light t))

;; Dark theme. Non-dark variant is found with spacemacs-light
 ;; alternative good option is tango-dark.
 ;; unsing t at the end supresses interactive prompt.

(use-package spacemacs-theme
      :defer t
      :init (progn
             (custom-set-variables '(spacemacs-theme-custom-colors
                                     '((bg1 .  "#f8f8f8"))
                            ))
             (load-theme 'spacemacs-light t)
             )
      )

;; (custom-set-variables '(spacemacs-theme-custom-colors
;;                           '(
;;                             (bg1 . "#ffffff")
;;                             )
;;                           )
;;                       )

(use-package flycheck
  :init
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))
