;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I'm relatively new to Emacs, so it's
;; probably terribad.

;;; Code:

;;; Package stuff
(require 'package)
;; Set package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
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
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package company
  :delight
  :hook (prog-mode . company-mode))

(use-package compile
  :init (setq compilation-scroll-output t))

(use-package counsel
  :bind (("M-x" . (lambda () (interactive) (counsel-M-x "")))
	 ("C-c u r" . counsel-rg))
  :config
  (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2)))

(use-package counsel-etags)

(use-package csv-mode)

(use-package dashboard
  :init
  (setq dashboard-items '(agenda projects))
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
  :delight)

(use-package emacs
  :delight emacs-lisp-mode
  :delight eshell-mode
  :delight subword-mode
  :delight whitespace-mode
  :delight abbrev-mode
  :hook (prog-mode . abbrev-mode)
  :hook (prog-mode . whitespace-mode)
  :hook (compilation-mode . visual-line-mode)
  :hook (text-mode . flyspell-mode)
  :bind ("C-c q" . browse-url)
  :config
  ;; Enable global modes
  (global-subword-mode)
  (column-number-mode)
  (show-paren-mode)
  (global-auto-revert-mode)
  (electric-pair-mode)
  (setq dired-listing-switches "-alh")
  (setq save-abbrevs 'silently)
  (setq window-buffer-change-functions '(balance-windows))
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(use-package ess)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-shell-name "/bin/bash")
  :config (exec-path-from-shell-initialize))

(use-package fish-mode)

(use-package flycheck
  :delight
  :hook (prog-mode . flycheck-mode))

(use-package go-mode)

(use-package haml-mode)

(use-package haskell-mode)

(use-package imenu
  :bind ("M-i" . imenu))

(use-package ivy
  :delight
  :init (setq ivy-use-virtual-buffers t)
  :bind ("C-c C-r" . ivy-resume)
  :config (ivy-mode 1))

(use-package linum
  :init (setq linum-format " %4d "))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :bind ("C-c l w s" . lsp-ivy-workspace-symbol))

(use-package lsp-mode
  :delight
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-restart 'auto-restart)
  (setq lsp-file-watch-threshold 50000)
  :hook
  (web-mode . lsp)
  (lsp-mode . lsp-headerline-breadcrumb-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-delay 2.0)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp :commands company-lsp)

(use-package spaceline
  :init (setq spaceline-minor-modes-p nil))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package nim-mode)

(use-package org
  :delight
  :delight org-indent-mode
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-switchb))
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode))
  :config
  (setq org-catch-invisible-edits 'smart)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-prefix-format
	'((agenda . " %i %-20:c%?-12t% s")
	  (todo . " %i %-12:c")
	  (tags . " %i %-12:c")
	  (search . " %i %-12:c"))))

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

(use-package pomodoro)

(use-package projectile
  :delight
  :init
  (setq-default projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action 'magit-status)
  (projectile-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package ripgrep)

(use-package rust-mode
  :delight)

(use-package sendmail
  :init (setq send-mail-function 'mailclient-send-it))

(use-package swiper
  :bind ("C-c s" . swiper))

(use-package tempo
  :bind
  ("C-c t c" . tempo-complete-tag)
  ("C-c t f" . tempo-forward-mark)
  ("C-c t b" . tempo-backward-mark)
  :config
  (setq tempo-interactive t))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . visual-line-mode)))

(use-package tide)

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.l?eex\\'" . web-mode))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'"))
  :hook
  (web-mode . tide-setup))

(use-package which-key
  :delight
  :config (which-key-mode t))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs trailing space-before-tab newline space-after-tab tab-mark)))

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

(setq custom-theme-directory (locate-user-emacs-file "themes"))
(load-theme 'wombat)
(spaceline-emacs-theme)

(defvar j3rn-bottom-windows
  '("\\*.?shell" "\\*elixir-test-output" "\\*exunit-compilation" "\\*Elixir" "\\*SQL")
  "A list of regular expressions that, if matched, will display the given buffer in the 'bottom'.")

(defun j3rn-bottom-window-p (buffer action)
  "Predicate indicating whether BUFFER should be placed at the bottom of the frame.  ACTION."
  (seq-some (lambda (pattern) (eq 0 (string-match-p pattern buffer))) j3rn-bottom-windows))

(setq display-buffer-alist
      '((j3rn-bottom-window-p display-buffer-in-side-window
			      (side . bottom)
			      (slot . -1)
			      (window-height . 0.5)
			      (inhibit-same-window . t))))

;;; Load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(load-library "ruby")
(load-library "elisp")
(load-library "elixir")
(load-library "git")
(if (locate-library "local") (load-library "local"))

;;; Startup
(server-start)			       ; Start the server so clients can connect

(provide 'init)
;;; init.el ends here
