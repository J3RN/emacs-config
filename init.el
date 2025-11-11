;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I use it.

;;; Code:

;;; Set my custom "light" theme as safe (this must come before auto-dark is loaded)
(setq custom-safe-themes '("dca48b51e11c5298236ca32aae33e5e72f8bf92dad65250506c7b3bd4a5145f0" default))

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

;; use-package declarations

(use-package async)

(use-package auto-dark
  :custom
  (auto-dark-themes '((wombat) (light)))
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

(use-package cook-mode
  :vc (:url "https://github.com/cooklang/cook-mode"
       :rev :newest
       :branch "master")
  :hook (cook-mode . visual-line-mode))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	    :rev :newest
	    :branch "main")
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

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
  (setq dashboard-agenda-sort-strategy '(time-up priority-up))
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-items '(agenda projects))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner (locate-user-emacs-file (file-name-concat "images" "128x128@2x.png")))
  :config
  (dashboard-setup-startup-hook))

(use-package delight)

(use-package dockerfile-mode)

(use-package doom-modeline
  :init
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (doom-modeline-mode 1))

(use-package eglot
  :bind
  ("C-c l r" . eglot-rename)
  ("C-c l f" . eglot-code-action-quickfix)
  ("M-+" . eglot-find-implementation)
  :config
  (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server" "--stdio")))

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
  :bind
  ("C-c q" . browse-url)
  ("C-c x s" . shell)
  ("C-c x e" . eshell)
  :config
  ;; Enable global modes
  (global-subword-mode)
  (column-number-mode)
  (show-paren-mode)
  (global-auto-revert-mode)
  (global-hi-lock-mode)
  (electric-pair-mode)
  ;; Configure built-ins
  (setq display-line-numbers-width-start t)
  (setq dired-listing-switches "-alh")
  (setq hexl-bits 8)
  (setq save-abbrevs 'silently)
  (setq window-buffer-change-functions '(balance-windows))
  (setq warning-minimum-level :error)
  (setq-default cursor-style 'bar)
  ;; No tabs is a good default
  (setq-default indent-tabs-mode nil)
  ;;; Enable useful disabled commands
  (put 'scroll-left 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package ess)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-shell-name "bash")
  :config (exec-path-from-shell-initialize))

(use-package fish-mode)

(use-package flymake
  :bind (("C-c ! n" . flymake-goto-next-error)
	 ("C-c ! p" . flymake-goto-prev-error)))

(use-package go-mode)

(use-package haml-mode)

(use-package imenu
  :bind ("M-i" . imenu))

(use-package ivy
  :delight
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d)")
  (ivy-mode 1))

(use-package ivy-hydra)

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
  (setq projectile-project-name-function (lambda (project-root)
					   (concat (file-name-as-directory (file-name-nondirectory (directory-file-name (file-name-parent-directory project-root))))
						   (file-name-nondirectory (directory-file-name project-root)))))
  (setq compilation-buffer-name-function (lambda (name-of-mode)
					   (if (equal (projectile-project-name) "-")
					       (compilation--default-buffer-name name-of-mode)
					     (concat "*compilation*<" (projectile-project-name) ">"))))
  (setq projectile-switch-project-action (lambda () (if (magit-toplevel) (magit-status) (dired "."))))
  (setq projectile-per-project-compilation-buffer t)
  (setq projectile-run-use-comint-mode t)
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

(use-package undo-tree
  :init
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-saves"))))
  :config (global-undo-tree-mode))

(use-package wat-ts-mode
  :vc (:url "https://github.com/J3RN/wat-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist '(wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))
  (add-to-list 'treesit-language-source-alist '(wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src")))

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
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'"))
  (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server" "--stdio"))
  :hook
  (web-mode . eglot-ensure)
  (web-mode . (lambda () (setq prettify-symbols-alist '(("->" . 8594)
                                                   ("=>" . 8658)
                                                   ("<=" . 8804)
                                                   (">=" . 8805)
                                                   ("||=" . 9568))))))

(use-package js
  :config
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode)))

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

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

;;; Bindings for built-in and custom functionality
;; *Unbind* C-z (suspend)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Delete trailing whitespace
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

;; Bind window-toggle-side-windows
(global-set-key (kbd "C-c 1") 'window-toggle-side-windows)

;; Package bindings
(global-set-key (kbd "C-c a l") 'package-list-packages)
(global-set-key (kbd "C-c a i") 'package-install)
(global-set-key (kbd "C-c a d") 'package-delete)

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

;;; Place lock files in /var/tmp
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

;;; Place auto-save files in /var/tmp
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

;;; Set sane scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; One line at a time
      mouse-wheel-progressive-speed nil            ; Don't accelerate scrolling
      mouse-wheel-follow-mouse t                   ; Scroll window mouse is positioned over
      mouse-wheel-tilt-scroll t                    ; Allow sideways scrolling
      mouse-wheel-flip-direction t                 ; This will forever bother me now that I've thought about it
      scroll-preserve-screen-position t            ; Keep the cursor at the same place when paging up or down
      scroll-conservatively 100)                   ; Scroll one line at a time when point moves off screen

;;; Confirm quitting Emacs (I accidentally cmd-q or C-x C-c sometimes)
(setq confirm-kill-emacs 'yes-or-no-p)

;;; Set shell font to be not bad
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

;;; Load libraries
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Programming, general
(load-library "git-config")
(load-library "hover")

;; Programming languages
(load-library "clojure-config")
(load-library "csharp-config")
(load-library "dhall-config")
(load-library "elisp-config")
(load-library "elixir-config")
(load-library "elm-config")
(load-library "fsharp-config")
(load-library "gleam-config")
(load-library "haskell-config")
(load-library "javascript-config")
(load-library "nix-config")
(load-library "ocaml-config")
(load-library "purescript-config")
(load-library "ruby-config")
(load-library "rust-config")

;; Miscellany
(load-library "diary-config")
(load-library "fast-buffers")
(when (display-graphic-p) (load-library "gui"))
;; Comment out if you don't like my layout
(load-library "j3rn-layout")
(load-library "tabs")
(load-library "yank-and-indent")

;; Local config
(when (locate-library "local") (load-library "local"))

(use-package hover
  :load-path "~/.config/emacs/elisp/hover.el"
  :hook (prog-mode . hover-mode))

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
