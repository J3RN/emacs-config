;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I use it.

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

;; Configure custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))

;; use-package declarations

(use-package async)

(use-package auto-dark
  :custom
  (auto-dark-light-theme 'adwaita)
  (auto-dark-dark-theme 'wombat)
  :config
  (auto-dark-mode 1))

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
  :bind (([remap execute-extended-command] . (lambda () (interactive) (counsel-M-x "")))
	 ([remap imenu] . counsel-imenu)
	 ("C-c u r" . counsel-rg))
  :config
  (setq counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading" "--line-number" "--hidden" "--color" "never" "%s"))
  (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2)))

(use-package counsel-etags)

(use-package csv-mode)

(use-package dashboard
  :init
  (setq dashboard-items '(agenda projects))
  (setq dashboard-startup-banner (locate-user-emacs-file "128x128@2x.png"))
  (setq dashboard-set-init-info nil)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  :config
  (dashboard-setup-startup-hook))

(use-package delight)

(use-package dictionary)

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
  (global-hi-lock-mode)
  (electric-pair-mode)
  (setq dired-listing-switches "-alh")
  (setq save-abbrevs 'silently)
  (setq window-buffer-change-functions '(balance-windows))
  (setq warning-minimum-level :error)
  ;;; Enable useful disabled commands
  (put 'scroll-left 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
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

(use-package imenu
  :bind ("M-i" . imenu))

(use-package ivy
  :delight
  :init (setq ivy-use-virtual-buffers t)
  :bind ("C-c C-r" . ivy-resume)
  :config (ivy-mode 1))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "%{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(use-package linum
  :init (setq linum-format " %4d "))
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
  (setq paradox-execute-asynchronously t)
  :bind
  ("C-c a l" . paradox-list-packages)
  ("C-c a i" . package-install))

(use-package phi-search
  :bind
  ([remap isearch-forward] . phi-search)
  ([remap query-replace] . phi-replace)
  ([remap query-replace-regexp] . phi-replace-query))

(use-package pomodoro)

(use-package projectile
  :delight
  :init
  (setq-default projectile-completion-system 'ivy)
  (setq projectile-generic-command "fd . -0 --type f --color=never")
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action (lambda () (if (magit-toplevel) (magit-status) (dired "."))))
  (projectile-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package restclient
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package ripgrep)

(use-package sendmail
  :init (setq send-mail-function 'mailclient-send-it))

(use-package spaceline
  :init
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-buffer-position-p nil)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-which-function-p t)
  (setq spaceline-hud-p t)
  :config
  ;; "theme" here just means it modifies the modeline.  Not to be confused with
  ;; the overall theme loaded by `load-theme'.
  (spaceline-emacs-theme))

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

(use-package tree-sitter-langs)

(use-package undo-tree
  :init
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-saves"))))
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
  (add-to-list 'auto-mode-alist '("\\.h?eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'"))
  :hook
  (web-mode . tide-setup))

(use-package which-func
  :config
  (set-face-attribute 'which-func nil :foreground "white")
  (which-function-mode t))

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

;; Bind window-toggle-side-windows
(global-set-key (kbd "C-c 1") 'window-toggle-side-windows)

;; Add kill defun function and binding
(defun kill-defun ()
  "Kill the function under point."
  (interactive)
  (mark-defun)
  (kill-region (point) (mark)))

;;; Disable bell altogether
(setq ring-bell-function (lambda () ()))

;;; Do not wrap long lines
(setq-default truncate-lines t)

;;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; GPG pinentry prompt fix for macOS
(setq-default epa-pinentry-mode 'loopback)

;;; ANSI colorize compilation output
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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; One line at a time
      mouse-wheel-progressive-speed nil            ; Don't accelerate scrolling
      mouse-wheel-follow-mouse t                   ; Scroll window mouse is positioned over
      mouse-wheel-tilt-scroll t                    ; Allow sideways scrolling
      mouse-wheel-flip-direction t                 ; This will forever bother me now that I've though about it
      scroll-conservatively 100)                   ; Scroll one line at a time when point moves off screen

;;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
(setq confirm-kill-emacs 'yes-or-no-p)

;;; Set shell font to be not bad
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

;;; Load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(when (display-graphic-p) (load-library "gui"))
(load-library "clojure")
(load-library "elisp")
(load-library "elixir")
(load-library "elm")
(load-library "fast-buffers")
(load-library "git")
(load-library "haskell")
(load-library "hover")
;; Comment out if you don't like my layout
(load-library "j3rn-layout")
(load-library "lsp")
(load-library "purescript")
(load-library "ruby")
(load-library "rust")
(load-library "tabs")
(load-library "yank-and-indent")

(when (locate-library "local") (load-library "local"))

;;; Startup
(server-start)			       ; Start the server so clients can connect

(provide 'init)
;;; init.el ends here
