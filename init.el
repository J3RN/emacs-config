;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I'm relatively new to Emacs, so it's
;; probably terribad.

;;; Code:

;;; Package stuff
(require 'package)
;; Use Melpa packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Use Elpa packages (default in versions 24+)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
;; Load and activate packages
(package-initialize)

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's missing
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Setup use-package
(require 'use-package)
(setq use-package-always-ensure t)	; Always ensure packages are installed
(setq use-package-always-demand t)	; Always eager load packages instead of lazy loading them

;; use-package declarations
(use-package alchemist
  :delight "🜂"
  :delight alchemist-phoenix-mode "🐦")


(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package bundler
  :bind (("C-c u i" . bundle-install)
	 ("C-c u o" . bundle-open)
	 ("C-c u u" . bundle-update)))

(use-package coffee-mode)

(use-package company
  :delight "🏢"
  :hook (prog-mode . company-mode))

(use-package compile
  :init (setq compilation-scroll-output t))

(use-package counsel
  :bind (("M-x" . (lambda () (interactive) (counsel-M-x "")))
	 ("C-c u r" . counsel-rg)))

(use-package counsel-etags)

(use-package csv-mode)

(use-package dashboard
  :init
  (setq dashboard-items '((projects . 5) (recents . 5)))
  (setq dashboard-startup-banner (locate-user-emacs-file "128x128@2x.png"))
  :config
  (dashboard-setup-startup-hook))

(use-package delight)

(use-package dictionary)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package dockerfile-mode)

(use-package eldoc
  :delight "🕮")

(use-package elixir-mode
  :delight "🜄"
  :bind ("C-c e f" . elixir-format))

(use-package emacs
  :delight emacs-lisp-mode "🐐"
  :delight eshell-mode "🐚"
  :delight subword-mode
  :delight whitespace-mode "»"
  :delight abbrev-mode "⫞"
  :hook (prog-mode . abbrev-mode)
  :hook (prog-mode . whitespace-mode)
  :config
  ;; Enable global modes
  (global-subword-mode)
  (column-number-mode)
  (show-paren-mode)
  (global-auto-revert-mode)
  (electric-pair-mode)
  (setq whitespace-style '(face tabs trailing space-before-tab newline empty space-after-tab tab-mark)))

(use-package enh-ruby-mode
  :delight "💎"
  :config
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
  :bind
  ("C-c r '" . ruby-toggle-string-quotes)
  ("C-c r {" . enh-ruby-toggle-block))

(use-package ess)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-shell-name "/bin/bash")
  :config (exec-path-from-shell-initialize))

(use-package fish-mode)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flycheck-mix
  :config (flycheck-mix-setup))

(use-package git-gutter)

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

(use-package go-mode)

(use-package haml-mode)

(use-package imenu
  :bind ("M-i" . imenu))

(use-package inf-ruby
  :config (setq inf-ruby-default-implementation "pry"))

(use-package ivy
  :delight
  :init (setq ivy-use-virtual-buffers t)
  :bind ("C-c C-r" . ivy-resume)
  :config (ivy-mode 1))

(use-package linum
  :init (setq linum-format " %4d "))

(use-package magit
  :bind
  ("C-c g s" . magit-status)
  ("C-c g a" . magit-dispatch-popup)
  ("C-c g f" . magit-file-popup)
  ("C-c g c" . magit-clone))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Workspace"))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package nim-mode)

(use-package org
  :delight "O"
  :delight org-indent-mode "→"
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-switchb))
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode))
  :config (setq org-catch-invisible-edits 'smart))

(use-package org-pomodoro
  :bind ("C-c o p" . org-pomodoro))

(use-package page-break-lines
  :delight
  :config
  (global-page-break-lines-mode))

(use-package paradox
  :init
  (setq paradox-automatically-star nil)
  (setq paradox-github-token t)
  :bind
  ("C-c a l" . paradox-list-packages)
  ("C-c a i" . package-install))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :delight "🄟"
  :bind (("C-c M-s" . paredit-splice-sexp)
	 ("C-S-<right>" . paredit-forward-slurp-sexp)
	 ("C-S-<left>" . paredit-forward-barf-sexp)
	 ("C-M-<right>" . paredit-backward-barf-sexp)
	 ("C-M-<left>" . paredit-backward-slurp-sexp)))

(use-package projectile
  :delight '(:eval (format "[%s]" (projectile-project-name)))
  :init
  (setq-default projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action 'magit-status)
  (projectile-mode))

(use-package projectile-rails
  :delight "🚈"
  :config
  (projectile-rails-global-mode)
  :hook (projectile-mode-hook . projectile-rails-on))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient)

(use-package ripgrep)

(use-package rspec-mode
  :delight "👓"
  :hook ((rspec-compilation-mode . visual-line-mode)
	 (projectile-rails-mode . rspec-mode)))

(use-package rubocop
  :bind (("C-c c c p" . rubocop-check-project)
	 ("C-c c c f" . rubocop-check-current-file)
	 ("C-c c a p" . rubocop-autocorrect-project)
	 ("C-c c a f" . rubocop-autocorrect-current-file)))

(use-package ruby-end
  :delight "⏎"
  :hook (elixir-mode . ruby-end-mode))

(use-package ruby-hash-syntax
  :config
  (define-key enh-ruby-mode-map (kbd "C-c h") 'ruby-hash-syntax-toggle))

(use-package ruby-test-mode)

(use-package rust-mode
  :delight "⚙")

(use-package sendmail
  :init (setq send-mail-function 'mailclient-send-it))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . visual-line-mode)))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'")))

(use-package which-key
  :delight
  :config (which-key-mode t))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yaml-mode)

;;; Bindings for built-in and custom functionality
;; *Unbind* C-z (suspend)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Delete trailing whitespace
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

;; Add kill defun function and binding
(defun kill-defun ()
  "Kill the function under point."
  (interactive)
  (mark-defun)
  (kill-region (point) (mark)))

(global-set-key (kbd "C-M-k") 'kill-defun)

;; Faster buffers
(global-set-key (kbd "C-c b") (lambda ()
				(interactive)
				(switch-to-buffer (other-buffer (current-buffer) 1))))
(global-set-key (kbd "C-c k") (lambda ()
				(interactive)
				(kill-buffer (current-buffer))))
(global-set-key (kbd "C-c 4 0") 'kill-buffer-and-window)
(global-set-key (kbd "C-c 4 b") (lambda ()
				  (interactive)
				  (switch-to-buffer-other-window (other-buffer (current-buffer) 1))))

;; Normal mac fullscreenifying shortcut
(if (string-equal "darwin" system-type)
    (progn
      (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
      (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)))

;;; Kill bell altogether
(setq ring-bell-function (lambda () ()))

;;; Do not wrap long lines
(setq-default truncate-lines t)

;;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; GPG pinentry prompt fix for macOS
(setq-default epa-pinentry-mode 'loopback)

;;; GUI Emacs
;; Disable toolbar
(tool-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Maximize the window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ANSI colorize compilation output
(defun colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in compilation output."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; Sensible backup settings
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; Set sane scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
      mouse-wheel-progressive-speed nil            ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                  ; scroll window under mouse
      scroll-conservatively 100)                   ; Scroll one line at a time when point moves off screen

;;; Enable useful disabled commands
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
(setq confirm-kill-emacs 'yes-or-no-p)

;;; Set shell font to be not bad
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

;;; Yank and indent
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (if (and (not current-prefix-arg)
		    (derived-mode-p 'prog-mode))
	       (let ((mark-even-if-inactive transient-mark-mode))
		 (indent-region (region-beginning) (region-end) nil))))))

;;; Load local configuration
(let ((local-config-file (locate-user-emacs-file "local.el")))
  (if (file-readable-p local-config-file)
      (load-file local-config-file)))

;;; Startup
(server-start)			       ; Start the server so clients can connect

(setq custom-theme-directory (locate-user-emacs-file "themes"))
(load-theme 'wombat)

(provide 'init)
;;; init.el ends here
