;;----------------------------------------------------------------------------
;; Init
;;----------------------------------------------------------------------------

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'delight)
  (package-install 'diminish)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'delight)
  (require 'use-package)
  (setq use-package-always-ensure t))

(setq custom-file "~/.emacs.d/custom.el")

;;----------------------------------------------------------------------------
;; Color Themes
;;----------------------------------------------------------------------------

(use-package color-theme-sanityinc-solarized
  :init (load-theme 'sanityinc-solarized-dark t))

(use-package color-theme-sanityinc-tomorrow)

;;----------------------------------------------------------------------------
;; Emacs Stuff
;;----------------------------------------------------------------------------

;; Basic Preferences
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(with-eval-after-load 'subword (diminish 'subword-mode))
(global-hl-line-mode)
(setq
 auto-revert-verbose nil
 frame-resize-pixelwise t
 frame-title-format '("%b")
 global-auto-revert-non-file-buffers t
 inhibit-splash-screen t
 inhibit-startup-message t
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
 ns-use-proxy-icon  nil
 uniquify-after-kill-buffer-p t
 uniquify-buffer-name-style 'reverse
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • ")
(setq-default
 column-number-mode t
 indent-tabs-mode nil
 indicate-empty-lines t
 make-backup-files nil)
(show-paren-mode 1)

;; Clear GUI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package anzu
  :init (add-hook 'after-init-hook 'global-anzu-mode)
  :diminish anzu-mode
  :config
  (set-face-attribute 'anzu-replace-highlight nil :background (face-attribute 'font-lock-string-face :foreground) :foreground (face-attribute 'default :background))
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package counsel
  :demand t
  :init (add-hook 'after-init-hook 'ivy-mode)
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
  :config
  (global-set-key [remap execute-extended-command] 'counsel-M-x)
  (setq
   counsel-find-file-occur-cmd "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d"
   ivy-height 20
   ivy-use-virtual-buffers t)
  (use-package flx)
  (use-package smex))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package flyspell
  :init (setenv "DICTIONARY" "en_GB")
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map ("C-;" . avy-goto-char))
  :config (add-hook 'text-mode-hook 'flyspell-mode)
  (use-package flyspell-correct-ivy
    :bind (:map flyspell-mode-map ("C-'" . flyspell-correct-previous-word-generic))))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (define-ibuffer-column size-h
    ;; Use human readable Size column instead of original one
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq
   ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process))
   ibuffer-filter-group-name-face 'font-lock-doc-face)
  (use-package ibuffer-vc
    :config (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)))

(use-package rg
  :bind ("C-c s" . rg))

(use-package scratch)

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq-default
   switch-window-shortcut-style 'alphabet
   switch-window-timeout nil))

(use-package wgrep
  :config (setq wgrep-enable-key "w"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

;;----------------------------------------------------------------------------
;; Editing Utils
;;----------------------------------------------------------------------------

(use-package avy
  :bind ("C-;" . avy-goto-char))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :diminish company-mode
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-tooltip-align-annotations t))
(defun s/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))

(use-package diff-hl
  :config (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))

(use-package evil
  :init (evil-mode t)
  :bind
  (:map evil-insert-state-map
        ([tab] . company-complete)
        :map evil-normal-state-map
        ([? ] . execute-extended-command)
        ([tab] . indent-for-tab-command)
        (";" . recenter-top-bottom)
        :map evil-visual-state-map
        ([? ] . execute-extended-command)
        :map ivy-occur-mode-map
        ("<return>" . ivy-occur-press-and-switch))
  :config (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (use-package evil-goggles
    :diminish evil-goggles-mode
    :config
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces))
  (use-package evil-mc
    :init
    (setq
     evil-mc-mode-line-text-cursor-color nil
     evil-mc-mode-line-text-inverse-colors nil
     evil-mc-one-cursor-show-mode-line-text nil)
    (global-evil-mc-mode 1)
    :bind
    (:map evil-normal-state-map
          ("g m m" . evil-mc-make-all-cursors)
          ("g m p" . evil-mc-pause-cursors)
          ("g m r" . evil-mc-resume-cursors)
          ("g m q" . evil-mc-undo-all-cursors)))
  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1))
  (use-package key-chord
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)))


(use-package expand-region
  :bind
  (("C--" . er/contract-region)
   ("C-=" . er/expand-region)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (setq flycheck-mode-line-prefix "F"))

(use-package highlight-symbol
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  :diminish highlight-symbol-mode)

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (global-auto-revert-mode)
  (diminish 'auto-revert-mode)
  (setq-default magit-diff-refine-hunk t)
  (use-package fullframe
    :config (fullframe magit-status magit-mode-quit-window)))

(use-package origami
  :bind
  (:map origami-mode-map
        ("C-c f" . origami-recursively-toggle-node)
        ("C-c F" . origami-toggle-all-nodes))
  :config (add-hook 'prog-mode-hook 'origami-mode))

(use-package projectile
  :init (add-hook 'after-init-hook 'projectile-global-mode)
  :delight '(:eval (format " P[%s]" (projectile-project-name)))
  :config (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
    :init (counsel-projectile-mode)
    :bind
    (:map counsel-projectile-command-map
          ("s" . counsel-projectile-rg))))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :init (add-hook 'after-init-hook 'smartparens-global-mode)
  :diminish smartparens-mode)

(use-package string-inflection
  :bind ("M-i". string-inflection-all-cycle))

(use-package undo-tree
  :diminish undo-tree-mode)

;; Whitespace stuff
(use-package whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  :diminish (whitespace-cleanup-mode whitespace-mode)
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq
   whitespace-line-column 80
   whitespace-style '(face lines-tail))
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

(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'ivy))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config (setq-default url-max-redirections 0)
  (use-package company-restclient
    :config (add-hook 'restclient-mode-hook (lambda () (s/local-push-company-backend 'company-restclient)))))


;;----------------------------------------------------------------------------
;; Programming Languages
;;----------------------------------------------------------------------------

;; ELisp
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

;; Golang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :bind
  (:map go-mode-map
        ("C-c C-t" . godef-describe)
        ("C-c C-d" . godoc-at-point))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (dolist (var '("GOBIN" "GOPATH")) (exec-path-from-shell-copy-env var))
  (setq gofmt-command "goimports")
  (use-package company-go
    :config
    (add-hook 'go-mode-hook (lambda () (s/local-push-company-backend 'company-go)))
    (custom-set-faces `(company-template-field
                        ((t (:background ,(face-attribute 'default :foreground)
                                         :foreground ,(face-attribute 'default :background))))))
    (setq company-go-show-annotation t))
  (use-package go-add-tags
    :bind (:map go-mode-map ("C-c t" . go-add-tags)))
  (use-package go-eldoc
    :config (add-hook 'go-mode-hook 'go-eldoc-setup))
  (use-package go-guru)
  (use-package golint))

(use-package protobuf-mode
  :init
  (add-hook 'protobuf-mode-hook 'highlight-symbol-mode)
  (add-hook 'protobuf-mode-hook 'highlight-symbol-nav-mode))

;; JSON
(use-package json-mode
  :config (setq js-indent-level 2))

;; nginx
(use-package nginx-mode)

;; Python
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :diminish anaconda-mode
  :config
  (use-package company-anaconda
    :config (add-hook 'python-mode-hook (lambda () (s/local-push-company-backend 'company-anaconda)))))

;; Web
(setq css-indent-offset 2)
(use-package css-eldoc
  :init (add-hook 'css-mode-hook 'turn-on-css-eldoc))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

(use-package web-mode
  :mode ("\\.js\\'" . web-mode)
  :config
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq
   web-mode-code-indent-offset 2
   web-mode-content-types-alist '(("jsx"  . "components\\/.*\\.js\\'"))
   web-mode-enable-auto-quoting nil
   web-mode-markup-indent-offset 2)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground (face-attribute 'font-lock-variable-name-face :foreground))
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'web-mode-html-tag-face nil :foreground (face-attribute 'default :foreground))
  (use-package add-node-modules-path
    :config (add-hook 'web-mode-hook #'add-node-modules-path))
  (use-package company-flow
    :config (add-hook 'web-mode-hook (lambda () (s/local-push-company-backend 'company-flow))))
  (use-package flycheck-flow
    :config
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
  (use-package prettier-js
    :diminish prettier-js-mode
    :config
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (setq prettier-js-args '("--trailing-comma" "all"))))


;;----------------------------------------------------------------------------
;; Writing
;;----------------------------------------------------------------------------

(use-package markdown-mode)

(use-package olivetti
  :bind (("C-c o" . olivetti-mode))
  :config (setq olivetti-body-width 82))

(put 'narrow-to-region 'disabled nil)
