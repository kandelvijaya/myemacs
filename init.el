(setq inhibit-startup-message t)  ;; hide startup msg
(scroll-bar-mode -1)              ;; disable visual scrollbar mode
(tool-bar-mode -1)                ;; disable toolbar
(tooltip-mode -1)                 ;; tooltip is a overlay info, disable it 
(set-fringe-mode 10)              ;; The fringe is a thin strip down the left and/or right edge of a window. 
(menu-bar-mode -1)                ;; disable menu bar mode


(set-face-attribute 'default nil :font "Fira Code" :height 150)

;; use M-x eval-buffer to run this.
;; use M-x describe-function to get help

;; TODO: symbol vs string on Lisp

;; Make ESC key work like CRTL + G
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
(unless package-archive-contents (package-install 'use-package) )

(require 'use-package)
(setq use-package-always-ensure t)

;; package to show command being used and what the function it calls
; (use-package command-log-mode)
;; didn't work

(use-package spacemacs-theme)

;; Dark theme. I would be okay with non-dark variant more so.
;; alternative good option is tango-dark. 
(load-theme 'spacemacs-dark)

;; Auto-Completion are provided by Helm or Ivy. It simplifies C-x C-f and file search,
;; fuzzy selection of commands on M-x
(use-package ivy
  :diminish				;doesnot show this one ModeLine to keep it simple.
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
     (ivy-mode 1)
  )



;; ivy should have installed these sub-dependencies but it didn't
(use-package counsel)
(use-package swiper)




;; Customizing ModeLine
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))


;; magit
(use-package magit)

