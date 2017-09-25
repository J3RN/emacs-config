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
(setq package-selected-packages
      '(counsel paradox go-mode dictionary neotree nim-mode projectile-ripgrep ripgrep rainbow-delimiters rust-mode rubocop org-bullets diff-hl monokai-theme org-projectile org-pomodoro smartparens which-key helm graphviz-dot-mode paredit projectile-rails ess flycheck znc yaml-mode web-mode ruby-test-mode org markdown-mode magit haml-mode git-gutter fish-mode exec-path-from-shell evil coffee-mode auto-complete auctex ag))

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

;;; Key bindings
;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Magit
(global-set-key (kbd "C-c m g") 'magit-status)
(global-set-key (kbd "C-c m f") 'magit-file-popup)
;; Org mode
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
;; Neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)
;; Counsel
(global-set-key (kbd "C-c u r") 'counsel-rg)
;; Custom
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
;; Enable shift-arrow keybindings for window movement
(when (fboundp 'windmove-default-keybindings)
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
;; Swiper replaces isearch
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-M-s") (lambda ()
			      (interactive)
			      (swiper (format "%s" (thing-at-point 'symbol)))))
(global-set-key (kbd "C-M-r") (lambda ()
			      (interactive)
			      (swiper (format "%s" (thing-at-point 'symbol)))))

;;; Do not wrap long lines
(setq-default truncate-lines t)

;;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; GPG pinentry prompt fix for macOS
(setq-default epa-pinentry-mode 'loopback)

;;; Global modes
;; Built-in
(column-number-mode)
(global-linum-mode)
(global-subword-mode)
(show-paren-mode)
;; Packages
(projectile-mode)
(projectile-rails-global-mode)
(helm-mode)
(global-flycheck-mode)
(which-key-mode)
(global-auto-complete-mode)
(global-diff-hl-mode)
(global-page-break-lines-mode)

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
;; Projectile
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;; Doc View
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; ERC
(add-hook 'erc-mode-hook 'visual-line-mode)
;; TeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; JavaScript
(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode nil)))
;; Org Bullets (pretty UTF-8 bullets for org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Show trailing whitespace in code files
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; Styling C
(add-hook 'c-mode-common-hook (lambda () (setq c-default-style "linux"
					       c-basic-offset 8
					       tab-width 8
					       indent-tabs-mode t)))

;;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)	 ;; scroll window under mouse

;;; Auto-mode adjustments
;; Treat SCSS as CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
;; Web-mode for web files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ; HTML
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))   ; PHP
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))   ; Handlebars
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))   ; ERB

;;; Sensible backup settings
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; Custom theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;; Add rg to projectile
(define-key projectile-command-map (kbd "s r") 'projectile-ripgrep)

;;; Allow 'vm rails' for the projectile-rails vanilla rails command
(add-to-list 'safe-local-variable-values
	     '(projectile-rails-vanilla-command . "vm rails"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (eminence)))
 '(custom-safe-themes
   (quote
    ("3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "390ede68e62beeb894d7b35c202153491774ad07a1e1e1745e679b5abd8c71be" "a1800595bab75fcf8085b3639142ca48b9d1d5cdc0d87defba091ed4912a03c5" "a49760e39bd7d7876c94ee4bf483760e064002830a63e24c2842a536c6a52756" "677177c27f2eb49e092e0e30813b9f2b25b3adc2484517f0f40e3722d999d628" "cc6cd39e0fe870d3dfe01577148299215f4f21e580ee49c229937f1a7fd12a3b" "7cf0f5bb26a1dc3fe14928caec3f96062409e52a394acbd8508b2d8f2d6367f7" "0df6738dfdef764770341bb1b6f8dd0bc53bae8423f3bf61a46c66cb8a8e7ca8" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "1fab355c4c92964546ab511838e3f9f5437f4e68d9d1d073ab8e36e51b26ca6a" "7bf64a1839bf4dbc61395bd034c21204f652185d17084761a648251041b70233" "63b7b8a45190b2e7362a975067bd76b55ae548c00e9088d12b4133eb0525a604" default)))
 '(erc-server-reconnect-attempts t)
 '(exec-path-from-shell-shell-name "/bin/bash")
 '(inhibit-startup-screen t)
 '(linum-format " %4d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(package-selected-packages
   (quote
    (dashboard page-break-lines counsel paradox go-mode dictionary neotree nim-mode projectile-ripgrep ripgrep rainbow-delimiters rust-mode rubocop org-bullets diff-hl monokai-theme org-projectile org-pomodoro smartparens which-key helm graphviz-dot-mode paredit projectile-rails ess flycheck znc yaml-mode web-mode ruby-test-mode org markdown-mode magit haml-mode git-gutter fish-mode exec-path-from-shell evil coffee-mode auto-complete auctex ag)))
 '(paradox-automatically-star nil)
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values
   (quote
    ((eval visual-line-mode t)
     (projectile-rails-vanilla-command . "vm rails"))))
 '(scroll-step 1)
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#2222FF"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#22A0F0"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#22F0F0"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#22F0A0"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#22FF22"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#A0F022"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#F0A022"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FF2222"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#F022A0")))))

;; Add to Auto-complete modes
(add-to-list 'ac-modes 'haml-mode)
(add-to-list 'ac-modes 'rust-mode)

;;; Allow scroll-left
(put 'scroll-left 'disabled nil)

;;; Package configuration
;; Initialize exec-path-from-shell
(exec-path-from-shell-initialize)
;; Autocomplete never ignores case
(setq-default ac-ignore-case nil)
;; Tell Projectile to use Helm for completion
(setq-default projectile-completion-system 'helm)
;; Silver Searcher configuration
(setq-default ag-reuse-window 't)
(paradox-upgrade-packages)		; Update packages with Paradox

;; Show startup screen
(dashboard-setup-startup-hook)

(provide 'init)
;;; init.el ends here
