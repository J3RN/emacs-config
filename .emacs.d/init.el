;;; init.el --- The startup file

;;; Commentary:
;; This is just my personal Emacs config.  I'm relatively new to Emacs, so it's
;; probably terribad.

;;; Code:

;;; Packages
(require 'package)
;; Use Melpa packages
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Use Elpa packages (default in versions 24+)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(package-initialize)

;;; Key bindings
;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Magit
(global-set-key (kbd "C-c m g") 'magit-status)
(global-set-key (kbd "C-c m b") 'magit-blame)
(global-set-key (kbd "C-c m d") 'magit-diff)
(global-set-key (kbd "C-c m c") 'magit-checkout)
;; Org mode
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
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

;;; Display items
;; Do not wrap long lines
(setq-default truncate-lines t)
;; Show trailing whitespace in code files
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;;; Better formatting of files
;; Require newlines at the end of files
(setq-default require-final-newline t)

;;; Global modes
(global-linum-mode)
(projectile-mode)
(helm-mode)
(global-flycheck-mode)
(projectile-rails-global-mode)
(global-subword-mode)
(which-key-mode)

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
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("7cf0f5bb26a1dc3fe14928caec3f96062409e52a394acbd8508b2d8f2d6367f7" "0df6738dfdef764770341bb1b6f8dd0bc53bae8423f3bf61a46c66cb8a8e7ca8" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "1fab355c4c92964546ab511838e3f9f5437f4e68d9d1d073ab8e36e51b26ca6a" "7bf64a1839bf4dbc61395bd034c21204f652185d17084761a648251041b70233" "63b7b8a45190b2e7362a975067bd76b55ae548c00e9088d12b4133eb0525a604" default)))
 '(erc-server-reconnect-attempts t)
 '(exec-path-from-shell-shell-name "/bin/bash")
 '(fringe-mode 6 nil (fringe))
 '(global-auto-complete-mode t)
 '(inhibit-startup-screen t)
 '(linum-format " %7d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(package-selected-packages
   (quote
    (dictionary neotree nim-mode projectile-ripgrep ripgrep rainbow-delimiters rust-mode rubocop org-bullets diff-hl monokai-theme org-projectile org-pomodoro smartparens which-key helm-projectile helm graphviz-dot-mode paredit projectile-rails ess flycheck znc yaml-mode web-mode ruby-test-mode org markdown-mode magit haml-mode git-gutter fish-mode exec-path-from-shell evil coffee-mode auto-complete auctex ag)))
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(projectile-mode t nil (projectile))
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

;;; Package configuration
;; Initialize exec-path-from-shell
(exec-path-from-shell-initialize)
;; Autocomplete never ignores case
(setq-default ac-ignore-case nil)
;; Tell Projectile to use Helm for completion
(setq-default projectile-completion-system 'helm)
;; Silver Searcher configuration
(setq-default ag-reuse-window 't)

(provide 'init)
;;; init.el ends here
