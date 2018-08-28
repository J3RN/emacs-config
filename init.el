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
(use-package alchemist)

(use-package auto-complete
  :config
  (global-auto-complete-mode)
  (setq-default ac-ignore-case nil)
  (add-to-list 'ac-modes 'haml-mode)
  (add-to-list 'ac-modes 'rust-mode))

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

(use-package compile
  :init (setq compilation-scroll-output t))

(use-package counsel
  :init (setq-default counsel-rg-base-command "rg -S -n --no-heading --color never %s .")
  :bind (("M-x" . counsel-M-x)
	 ("C-c u r" . counsel-rg)))

(use-package counsel-etags)

(use-package csv-mode)

(use-package dashboard
  :init
  (setq dashboard-items '((projects . 5) (recents . 5)))
  (setq dashboard-startup-banner "~/.emacs.d/256x256@2x.png")
  :config
  (dashboard-setup-startup-hook))

(use-package dictionary)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package dockerfile-mode)

(use-package elixir-mode)

(use-package enh-ruby-mode
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
  :config
  (global-flycheck-mode))

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
  :init (setq ivy-use-virtual-buffers t)
  :bind ("C-c C-r" . ivy-resume)
  :config (ivy-mode 1))

(use-package linum
  :init (setq linum-format " %4d "))

(use-package magit
  :bind
  ("C-c g s" . magit-status)
  ("C-c g a" . magit-dispatch-popup)
  ("C-c g f" . magit-file-popup))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Workspace"))

(use-package markdown-mode
  :hook ('markdown-mode . 'visual-line-mode))

(use-package monokai-theme
  :config
  (setq custom-safe-themes '("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default))
  (load-theme 'monokai))

(use-package nim-mode)

(use-package org
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-switchb))
  :hook (('org-mode . 'visual-line-mode)
	 ('org-mode . 'org-indent-mode))
  :config (setq org-catch-invisible-edits 'smart))

(use-package org-pomodoro
  :bind ("C-c o p" . org-pomodoro))

(use-package page-break-lines
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
  :hook (emacs-lisp-mode . paredit-mode))

(use-package projectile
  :init
  (setq-default projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action 'magit-status)
  (projectile-mode))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  :hook ('projectile-mode-hook . 'projectile-rails-on))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient)

(use-package ripgrep)

(use-package rspec-mode
  :hook ((rspec-compilation-mode . visual-line-mode)
	 (projectile-rails-mode . rspec-mode)))

(use-package rubocop)

(use-package ruby-end
  :hook (elixir-mode . ruby-end-mode))

(use-package ruby-hash-syntax
  :config
  (define-key enh-ruby-mode-map (kbd "C-c h") 'ruby-hash-syntax-toggle))

(use-package ruby-test-mode)

(use-package rust-mode)

(use-package sendmail
  :init (setq send-mail-function 'mailclient-send-it))

(use-package smartparens
  :config
  (smartparens-global-mode)
  (define-key smartparens-mode-map (kbd "C-c {") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c }") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c `") 'sp-unwrap-sexp))

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
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode)))

(use-package which-key
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

;; Kill defun
(global-set-key (kbd "C-M-k")
		(lambda ()
		  (interactive)
		  (mark-defun)
		  (kill-region (point) (mark))))

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

;;; Use visible bell instead of audible one for a quiet editing experience
(setq visible-bell t)

;;; Do not wrap long lines
(setq-default truncate-lines t)

;;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; GPG pinentry prompt fix for macOS
(setq-default epa-pinentry-mode 'loopback)

;;; Enable built-in global modes
(column-number-mode)
(global-subword-mode)
(show-paren-mode)
(global-auto-revert-mode)
(electric-pair-mode)

;;; GUI Emacs
;; Disable toolbar
(tool-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Maximize the window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Hooks
;; Show trailing whitespace in code files
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; Styling C
(add-hook 'c-mode-common-hook (lambda () (setq c-default-style "linux"
					       c-basic-offset 8
					       tab-width 8
					       indent-tabs-mode t)))

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
(put 'scroll-left 'disabled nil)                   ; Allow scroll-left

;;; Always split vertically
(setq split-width-threshold nil)

;;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
(setq confirm-kill-emacs 'yes-or-no-p)

;;; Load local configuration
(let ((local-config-file (locate-user-emacs-file "local.el")))
  (if (file-readable-p local-config-file)
      (load-file local-config-file)))

;;; Startup
(server-start)			       ; Start the server so clients can connect

(provide 'init)
;;; init.el ends here
