;;----------------------------------------------------------------------------
;; Init
;;----------------------------------------------------------------------------

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'delight)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'delight)
  (require 'diminish)
  (setq use-package-always-ensure t))

(setq custom-file "~/.emacs.d/custom.el")


;;----------------------------------------------------------------------------
;; Color Themes
;;----------------------------------------------------------------------------

(use-package color-theme-sanityinc-solarized
  :init
  (load-theme 'sanityinc-solarized-dark t))

;;----------------------------------------------------------------------------
;; Emacs Stuff
;;----------------------------------------------------------------------------

;; Clear GUI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package counsel
  :demand t
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x f" . counsel-recentf)
	 :map ivy-minibuffer-map
	 ("<return>" . ivy-alt-done)
	 ("C-j" . ivy-immediate-done)
	 :map swiper-map
	 ("C-r" . ivy-previous-line))
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (global-set-key [remap execute-extended-command] 'counsel-M-x)
  (setq ivy-height 20
	ivy-use-virtual-buffers t)
  (use-package flx)
  (use-package smex))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package rg
  :bind ("M-s" . rg))

;;----------------------------------------------------------------------------
;; Editing Utils
;;----------------------------------------------------------------------------

(global-hl-line-mode)
(show-paren-mode 1)

(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	:map evil-insert-state-map
	([tab] . company-complete))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))

(use-package evil
  :init (evil-mode t)
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk t))
(diminish 'auto-revert-mode)
(global-auto-revert-mode)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(use-package projectile
  :delight '(:eval (format " P[%s]" (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-global-mode)
  :config
  (use-package counsel-projectile
    :bind ("C-c s" . counsel-projectile-rg))
  (setq projectile-completion-system 'ivy))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package whitespace-cleanup-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))
(setq-default show-trailing-whitespace t)

;;----------------------------------------------------------------------------
;; Programming Languages
;;----------------------------------------------------------------------------

;; ELisp
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package smartparens
  :init
  (add-hook 'after-init-hook 'smartparens-global-mode))
