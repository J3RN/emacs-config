;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I'm relatively new to Emacs, so it's
;; probably terribad.

;;; Code:

;;; Load local configuration
(let ((local-config-file (locate-user-emacs-file "local.el")))
  (if (file-readable-p local-config-file)
      (load-file local-config-file)))

;;; Packages
(setq package-selected-packages '(ag auctex auto-complete bundler coffee-mode counsel counsel-etags csv-mode dashboard dictionary diff-hl enh-ruby-mode ess evil exec-path-from-shell fish-mode flycheck git-gutter git-timemachine go-mode graphviz-dot-mode haml-mode helm magit magithub markdown-mode monokai-theme neotree nim-mode org org-bullets org-pomodoro page-break-lines paradox paredit projectile-rails projectile-ripgrep rainbow-delimiters restclient ripgrep rubocop ruby-end ruby-hash-syntax ruby-test-mode rust-mode smartparens spacemacs-theme use-package web-mode which-key yaml-mode zenburn-theme))

(require 'package)
;; Use Melpa packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Use Elpa packages (default in versions 24+)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(package-initialize)			; Load and activate packages

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Always demand. Eager load packages instead of lazy loading them.
(setq use-package-always-demand t)

;; use-package declarations
(use-package auto-complete
  :config
  (global-auto-complete-mode)
  (setq-default ac-ignore-case nil)
  (add-to-list 'ac-modes 'haml-mode)
  (add-to-list 'ac-modes 'rust-mode))

(use-package bundler
  :bind ("C-c u i" . bundle-install))

(use-package compile
  :init (setq compilation-scroll-output t)
  :bind ("C-c c" . compile))

(use-package counsel
  :init (setq-default counsel-rg-base-command "rg -S -n --no-heading --color never %s .")
  :bind (("M-x" . counsel-M-x)
	 ("C-c u r" . counsel-rg)))

(use-package dashboard
  :init
  (setq dashboard-items '((projects . 5) (recents . 5)))
  (setq dashboard-startup-banner "~/.emacs.d/256x256@2x.png")
  :config
  (dashboard-setup-startup-hook))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package enh-ruby-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode)))

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-shell-name "/bin/bash")
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package ivy
  :init (setq ivy-use-virtual-buffers t)
  :bind ("C-c C-r" . ivy-resume)
  :config (ivy-mode 1))

(use-package linum
  :init (setq linum-format " %4d ")
  :config (global-linum-mode))

(use-package magit
  :bind (("C-c g s" . magit-status)
	 ("C-c g a" . magit-dispatch-popup)
	 ("C-c g f" . magit-file-popup)
	 ("C-c g t" . git-timemachine)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Workspace"))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package mwheel
  :init
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't))                 ;; scroll window under mouse

(use-package neotree
  :bind ("C-c n" . neotree-toggle))

(use-package org
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-iswitchb))
  :config (add-hook 'org-mode-hook 'visual-line-mode))

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

(use-package projectile
  :init
  (setq-default projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  (define-key projectile-command-map (kbd "s r") 'projectile-ripgrep))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package sendmail
  :init (setq send-mail-function 'mailclient-send-it))

(use-package ruby-hash-syntax
  :config
  (define-key enh-ruby-mode-map (kbd "C-c h") 'ruby-hash-syntax-toggle))

(use-package simple
  :bind ("C-c d" . delete-trailing-whitespace))

(use-package smartparens
  :config
  (smartparens-global-mode)
  (define-key smartparens-mode-map (kbd "C-c {") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c }") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c `") 'sp-unwrap-sexp))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

(use-package which-key
  :config
  (which-key-mode))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package windmove
  :config
  (windmove-default-keybindings))

;; *Unbind* C-z (suspend)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
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

;;; Use visible bell instead of audible one for a quiet editing experience
(setq visible-bell t)

;;; Do not wrap long lines
(setq-default truncate-lines t)

;;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; GPG pinentry prompt fix for macOS
(setq-default epa-pinentry-mode 'loopback)

;;; Global modes
;; Built-in
(column-number-mode)
(global-subword-mode)
(show-paren-mode)
(global-auto-revert-mode)

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
;; Doc View
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; TeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; Org Bullets (pretty UTF-8 bullets for org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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

;; Set Monokai as the current theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-safe-themes '("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default))
(load-theme 'monokai)

;; Scrolling
(setq scroll-conservatively 100) ; Scroll one line at a time when point moves off screen
(put 'scroll-left 'disabled nil) ; Allow scroll-left

;; Always split vertically
(setq split-width-threshold nil)

;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
(setq confirm-kill-emacs 'yes-or-no-p)

;; Startup
(paradox-upgrade-packages)	       ; Upgrade packages with Paradox
(server-start)			       ; Start the server so clients can connect

(provide 'init)
;;; init.el ends here
