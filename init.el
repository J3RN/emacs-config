;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I use it.

;;; Code:

;;; Package stuff
(require 'package)
;; Set package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Load and activate packages
(package-initialize)

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's missing
;; use-package is included in Emacs 29.1 and later
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Setup use-package
(require 'use-package)
(setq use-package-always-ensure t)	; Always ensure packages are installed
(setq use-package-always-demand t)	; Always eager load packages instead of lazy loading them

;; Configure custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
;; Set my custom "light" theme as safe (this must come before auto-dark is loaded)
(setq custom-safe-themes '("dca48b51e11c5298236ca32aae33e5e72f8bf92dad65250506c7b3bd4a5145f0" default))

;; use-package declarations

(use-package abbrev
  :ensure nil
  :delight
  :config
  (setq save-abbrevs 'silently)
  :hook
  (prog-mode . abbrev-mode))

(use-package async)

(use-package auto-dark
  :delight
  :custom
  (auto-dark-themes '((wombat) (light)))
  :config
  (auto-dark-mode 1))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode))

(use-package browse-url
  :ensure nil
  :bind ("C-c q" . browse-url))

(use-package company
  :delight
  :hook (prog-mode . company-mode))

(use-package compile
  :ensure nil
  :init (setq compilation-scroll-output t))

(use-package cook-mode
  :vc (:url "https://github.com/cooklang/cook-mode"
       :rev :newest
       :branch "master")
  :hook (cook-mode . visual-line-mode))

(use-package csv-mode)

(use-package dashboard
  :init
  (setq dashboard-agenda-sort-strategy '(time-up priority-up))
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-items '(agenda projects))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner (locate-user-emacs-file (file-name-concat "images" "128x128@2x.png")))
  :config
  (dashboard-setup-startup-hook))

(use-package delight)

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh"))

(use-package display-line-numbers
  :ensure nil
  :config
  (setq display-line-numbers-width-start t))

(use-package dockerfile-mode)

(use-package doom-modeline
  :init
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (doom-modeline-mode 1))

(use-package eglot
  :ensure nil
  :bind
  ("C-c l r" . eglot-rename)
  ("C-c l f" . eglot-code-action-quickfix)
  ("M-+" . eglot-find-implementation))

(use-package eldoc
  :ensure nil
  :delight)

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode))

(use-package emacs
  :config
  ;; Disable bell altogether
  (setq ring-bell-function (lambda () ()))
  ;; Balance windows when making or deleting windows
  (setq window-buffer-change-functions '(balance-windows))
  ;; Do not wrap long lines by default
  (setq-default truncate-lines t)
  ;;; Enable useful disabled commands
  (put 'scroll-left 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; *Unbind* C-z (suspend)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package epg-config
  :ensure nil
  :config
  ;; GPG pinentry prompt fix for macOS
  (setq-default epa-pinentry-mode 'loopback))

(use-package eshell
  :ensure nil
  :bind
  ("C-c x e" . eshell)
  :hook
  (eshell-mode . visual-line-mode))

(use-package ess)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-shell-name "bash")
  :config (exec-path-from-shell-initialize))

(use-package fish-mode)

(use-package flymake
  :ensure nil
  :bind (("C-c ! n" . flymake-goto-next-error)
	 ("C-c ! p" . flymake-goto-prev-error)))

(use-package flyspell
  :ensure nil
  :hook
  (text-mode . flyspell-mode))

(use-package files
  :ensure nil
  :config
  ;; Require newlines at the end of files
  (setq-default require-final-newline t)
  ;; Sensible backup settings
  (setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
  ;; Place lock files in /var/tmp
  (setq lock-file-name-transforms
        '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
  ;; Place auto-save files in /var/tmp
  (setq auto-save-file-name-transforms
        '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
  ;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
  (setq confirm-kill-emacs 'yes-or-no-p))

(use-package go-mode)

(use-package haml-mode)

(use-package hexl
  :ensure nil
  :config
  (setq hexl-bits 8))

(use-package hi-lock
  :config
  (global-hi-lock-mode))

(use-package hover
  :hook (prog-mode . hover-mode))

(use-package icomplete
  :ensure nil
  :config
  (icomplete-vertical-mode)
  (setq completion-styles '(basic substring flex))
  (setq read-buffer-completion-ignore-case t)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-scroll t)
  :bind (:map icomplete-minibuffer-map
              ("RET" . icomplete-force-complete-and-exit)
              ("C-j" . exit-minibuffer)))

(use-package imenu
  :ensure nil
  :bind ("M-i" . imenu))

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
                                       "\\\\" "://" "::<" "www"))
  (global-ligature-mode t))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
	 (markdown-mode . whitespace-mode)))

(use-package nim-mode)

(use-package org
  :ensure nil
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

;; A bit meta, eh?
(use-package package
  :bind
  ("C-c a l" . package-list-packages)
  ("C-c a i" . package-install)
  ("C-c a d" . package-delete))

(use-package page-break-lines
  :delight
  :config
  (global-page-break-lines-mode))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode))

(use-package phi-search
  :bind
  ([remap isearch-forward] . phi-search)
  ([remap query-replace] . phi-replace)
  ([remap query-replace-regexp] . phi-replace-query))

(use-package pomodoro)

(use-package project
  :ensure nil
  :config
  (cl-defmethod project-name (project)
    (let ((root (project-root project)))
      (concat (file-name-as-directory (file-name-nondirectory (directory-file-name (file-name-parent-directory root))))
              (file-name-nondirectory (directory-file-name root))))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

(use-package restclient
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package ripgrep)

(use-package sendmail
  :ensure nil
  :init (setq send-mail-function 'mailclient-send-it))

(use-package swiper
  :bind ("C-c s" . swiper))

(use-package shell
  :bind
  ("C-c x s" . shell)
  :hook
  (shell-mode . visual-line-mode))

(use-package simple
  :ensure nil
  :config
  ;; Show column number in modeline
  (column-number-mode)
  ;; No tabs is a good default
  (setq-default indent-tabs-mode nil)
  :bind
  ("C-c d" . delete-trailng-whitespace)
  :hook
  (compilation-mode . visual-line-mode))

(use-package subword
  :ensure nil
  :delight
  :config (global-subword-mode))

(use-package tempo
  :ensure nil
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

(use-package undo-tree
  :init
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-saves"))))
  :config (global-undo-tree-mode))

(use-package warnings
  :ensure nil
  :config
  (setq warning-minimum-level :error))

(use-package wat-ts-mode
  :vc (:url "https://github.com/J3RN/wat-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist '(wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))
  (add-to-list 'treesit-language-source-alist '(wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src")))

(use-package which-func
  :ensure nil
  :config
  (set-face-attribute 'which-func nil :foreground "white")
  (which-function-mode t))

(use-package which-key
  :ensure nil
  :delight
  :config (which-key-mode t))

(use-package whitespace
  :ensure nil
  :delight
  :bind ("C-c w" . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs trailing space-before-tab newline space-after-tab tab-mark))
  :hook
  (prog-mode . whitespace-mode))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package window
  :bind
  ("C-c 1" . window-toggle-side-windows))

(use-package yaml-mode)

(use-package yasnippet
  :delight yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

;; Add kill defun function and binding
(defun kill-defun ()
  "Kill the function under point."
  (interactive)
  (mark-defun)
  (kill-region (point) (mark)))

;;; ANSI colorize compilation output
(defun colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in compilation output."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; Set sane scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; One line at a time
      mouse-wheel-progressive-speed nil            ; Don't accelerate scrolling
      mouse-wheel-follow-mouse t                   ; Scroll window mouse is positioned over
      mouse-wheel-tilt-scroll t                    ; Allow sideways scrolling
      mouse-wheel-flip-direction t                 ; Emacs treats trackpad and mouse wheel differently, preserve trackpad sanity
      scroll-conservatively 100)                   ; Scroll one line at a time when point moves off screen

;;; Load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Programming, general
(load-library "j3rn-git")

;; Programming languages
(load-library "j3rn-clojure")
(load-library "j3rn-csharp")
(load-library "j3rn-dhall")
(load-library "j3rn-elisp")
(load-library "j3rn-elixir")
(load-library "j3rn-elm")
(load-library "j3rn-fsharp")
(load-library "j3rn-gleam")
(load-library "j3rn-haskell")
(load-library "j3rn-javascript")
(load-library "j3rn-nix")
(load-library "j3rn-ocaml")
(load-library "j3rn-purescript")
(load-library "j3rn-ruby")
(load-library "j3rn-rust")
(load-library "j3rn-typescript")

;; Miscellany
(load-library "j3rn-diary")
(load-library "fast-buffers")
(when (display-graphic-p) (load-library "j3rn-gui"))
;; Comment out if you don't like my layout
(load-library "j3rn-layout")
(load-library "j3rn-tabs")
(load-library "yank-and-indent")

;; Local config
(when (locate-library "local") (load-library "local"))

;;; Startup
(server-start)			       ; Start the server so clients can connect

;;; Restore original GC values (borrowed from redguardtoo/emacs.d)
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

(provide 'init)
;;; init.el ends here
