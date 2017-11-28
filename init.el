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

(use-package color-theme-sanityinc-tomorrow)

;;----------------------------------------------------------------------------
;; Emacs Stuff
;;----------------------------------------------------------------------------

;; Clear GUI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Basic Preferences
(add-hook 'after-init-hook 'winner-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(diminish 'abbrev-mode)
(global-hl-line-mode)
(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 uniquify-after-kill-buffer-p t
 uniquify-buffer-name-style 'reverse
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • ")
(setq-default
 column-number-mode t)
(show-paren-mode 1)

(use-package anzu
  :diminish anzu-mode
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package counsel
  :demand t
  :diminish ivy-mode
  :bind
  (("C-s" . counsel-grep-or-swiper)
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
  (global-set-key (kbd "C-c C-m") 'execute-extended-command)
  (global-set-key [remap execute-extended-command] 'counsel-M-x)
  (setq ivy-height 20
	ivy-use-virtual-buffers t)
  (use-package flx)
  (use-package smex))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuffer-vc
    :config (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))
  (require 'ibuffer)
  (require 'ibuffer-vc)
  (define-ibuffer-column size-h
    ;; Use human readable Size column instead of original one
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
  	'((mark modified read-only vc-status-mini " "
  		(name 18 18 :left :elide)
  		" "
  		(size-h 9 -1 :right)
  		" "
  		(mode 16 16 :left :elide)
  		" "
  		filename-and-process)))
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(use-package rg
  :bind ("C-c s" . rg))

(use-package scratch)

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil))


(use-package wgrep
  :config (setq wgrep-enable-key "w"))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

;;----------------------------------------------------------------------------
;; Editing Utils
;;----------------------------------------------------------------------------

(use-package avy
  :bind ("C-;" . avy-goto-char))

(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . company-select-next)
	("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
	company-idle-delay 0))
(defun s/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))

(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package evil
  :init (evil-mode t)
  :bind
  (:map evil-insert-state-map
	([tab] . company-complete)
	:map evil-normal-state-map
	([tab] . indent-for-tab-command)
	:map ivy-occur-mode-map
	("<return>" . ivy-occur-press-and-switch))
  :config
  (use-package evil-goggles
    :diminish evil-goggles-mode
    :config
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces))
  (use-package evil-mc
    :init
    (setq evil-mc-mode-line-text-cursor-color nil)
    (setq evil-mc-mode-line-text-inverse-colors nil)
    (setq evil-mc-one-cursor-show-mode-line-text nil)
    (global-evil-mc-mode 1)
    :bind
    (:map evil-normal-state-map
	  ("g m m" . evil-mc-make-all-cursors)
	  ("g m p" . evil-mc-pause-cursors)
	  ("g m r" . evil-mc-resume-cursors)
	  ("g m q" . evil-mc-undo-all-cursors)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config (setq flycheck-mode-line-prefix "F"))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (use-package fullframe
    :config (fullframe magit-status magit-mode-quit-window))
  (setq-default magit-diff-refine-hunk t))
(global-auto-revert-mode)
(diminish 'auto-revert-mode)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(use-package projectile
  :delight '(:eval (format " P[%s]" (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-global-mode)
  :config
  (use-package counsel-projectile
    :bind
    (:map projectile-command-map
	  ("s" . counsel-projectile-rg))
    :config (counsel-projectile-on))
  (setq projectile-completion-system 'ivy))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'after-init-hook 'smartparens-global-mode))

(use-package string-inflection
  :bind ("M-i". string-inflection-all-cycle))

(use-package undo-tree
  :diminish undo-tree-mode)

;; Whitespace stuff
(use-package whitespace-cleanup-mode
  :diminish (whitespace-cleanup-mode
	     whitespace-mode)
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (setq-default show-trailing-whitespace t))
(defun s/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))
(dolist (hook '(magit-popup-mode-hook
		special-mode-hook
                Info-mode-hook
                compilation-mode-hook
                eww-mode-hook
                minibuffer-setup-hook
                term-mode-hook))
  (add-hook hook #'s/no-trailing-whitespace))

;;----------------------------------------------------------------------------
;; Programming Utils
;;----------------------------------------------------------------------------

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (use-package company-restclient
    :config (add-hook 'restclient-mode-hook (lambda () (s/local-push-company-backend 'company-restclient)))))


;;----------------------------------------------------------------------------
;; Programming Languages
;;----------------------------------------------------------------------------

;; ELisp
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

;; JS
(defun s/use-eslint-from-node-modules ()
  "Use local eslint from node_modules."
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node_modules"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js"
					root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(setq css-indent-offset 2)

(use-package web-mode
  :mode ("\\.js[x]?\\'" . web-mode)
  :config
  (use-package json-mode)
  (use-package prettier-js
    :diminish prettier-js-mode
    :config
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (setq prettier-js-args '("--trailing-comma" "es5")))
  (use-package rainbow-mode
    :diminish rainbow-mode
    :config
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'web-mode-hook 'rainbow-mode))
  (use-package tern
    :diminish tern-mode
    :config
    (add-hook 'web-mode-hook 'tern-mode)
    (use-package company-tern
      :config
      (add-hook 'web-mode-hook (lambda () (s/local-push-company-backend 'company-tern)))
      (setq company-tern-property-marker " <p>")))
  (add-hook 'flycheck-mode-hook #'s/use-eslint-from-node-modules)
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground (face-attribute 'font-lock-variable-name-face :foreground))
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'web-mode-html-tag-face nil :foreground (face-attribute 'default :foreground)))

;; Python

(use-package anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (use-package company-anaconda
    :config (add-hook 'python-mode-hook (lambda () (s/local-push-company-backend 'company-anaconda)))))

;;----------------------------------------------------------------------------
;; Writing
;;----------------------------------------------------------------------------

(use-package olivetti
  :bind
  (("C-c o" . olivetti-mode)
   ("C-c m" . olivetti-toggle-hide-mode-line)))
