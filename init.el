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

;; download Fira code fonts which looks good. 
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

;; Customizing ModeLine with all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

(defun org-babel-tangle-emacsconfig-on-save ()
    (when (string-equal (buffer-file-name)
			(expand-file-name "~/.emacs.d/EmacsConfig.org"))
      (let ((org-config-babel-evaluate nil))
	(org-babel-tangle))))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook #'org-babel-tangle-emacsconfig-on-save)))

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

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; enable doc lookup popup on the side
(use-package corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)


;; move corfu popup to mini-buffer for more actions that can be performed. 
(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

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

(use-package f)

(use-package s)

(use-package ag)

(defun bj-close-all-other-buffers-if-not-modified ()
  "Kill all unmodified buffer besides the current one."
  (interactive )
  (seq-map
   #'(lambda (b) (kill-buffer-if-not-modified b))
   (seq-filter #'(lambda (x) (not (equal (current-buffer) x))) (buffer-list))
   )
  (bj-close-all-other-**buffers)
  )

(defun bj-close-all-other-**buffers ()
  "Kills all sorts of *Messgaes* *Helpful* buffer besides this one"
  (interactive)
  (seq-map
   #'(lambda (b) (kill-buffer b))
   (seq-filter
    #'(lambda (b)  (and (not (equal (current-buffer) b)) (s-prefix? "*" (buffer-name b)) (s-suffix? "*" (buffer-name b)) ) )
    (buffer-list)))
  )

(global-set-key (kbd "C-x a k") 'bj-close-all-other-buffers-if-not-modified)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t j"   . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
        ;; my custom bindings
        ;; This replaces treemacs-project-up/down commands which I am not using
        :map treemacs-mode-map
        ("<M-up>" . treemacs-root-up)
        ("<M-down>" . treemacs-root-down) 
  ))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; line and column numbers show
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numebrs for specific modes
(dolist (mode '(
                ; list of modes 
                org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
         (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; if all-icons-font is not installed prompt the user to install
;; manula installation via `M-x all-the-icons-install-fonts`
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))

(setq debug-on-error t)

(use-package xwwp)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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



(setq lsp-clients-python-library-directories '("/opt/homebrew/lib/python3.10/site-packages/"))

(use-package json-mode)

(setq default-directory "~/")



(use-package paredit)

(defun my/autoparens () (paredit-mode t))
(add-hook 'emacs-lisp-mode-hook #'my/autoparens)
(add-hook 'lisp-interaction-mode-hook #'my/autoparens)

(use-package aggressive-indent)
(defun bj/aggressive-indent-enabled () (aggressive-indent-mode t))
(add-hook 'emacs-lisp-mode #'bj/aggressive-indent-enabled)
(add-hook 'lisp-interaction-mode-hook #'bj/aggressive-indent-enabled)


;; issue with indenting in org mode, esp code blocks inside. 
;; (add-hook 'org-mode-hook #'bj/aggressive-indent-enabled)

(use-package org-roam-ui)

(setq org-agenda-files (directory-files-recursively "~/Library/Mobile Documents/com~apple~CloudDocs/All Notes/roamnotes/" "\\.org$"))

(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-log-done 'time)

(setq org-agenda-follow-mode 't)

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

(setenv "PAGER" "cat")

(use-package tldr)

(setq eshell-directory-name ".cache/eshell")

(use-package typescript-mode)

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

(use-package org-modern)
(global-org-modern-mode)

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
