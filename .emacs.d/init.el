;;; Packages
(require 'package)
;; Use Melpa packages
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Use Elpa packages (default in versions 24+)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(package-initialize)

;;; Package configuration
;; Initialize exec-path-from-shell
(exec-path-from-shell-initialize)
;; Autocomplete never ignores case
(setq-default ac-ignore-case nil)

;;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") (lambda () (interactive) (shell)))
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
;; Enable shift-arrow keybindings for window movement
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; Org mode recommendations
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
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
;; Line numbers
(global-linum-mode t)
;; Projectile
(projectile-mode t)
;; Ido mode
(ido-mode t)
;; Flycheck mode
(global-flycheck-mode t)
;; Projectile-rails mode
(projectile-rails-global-mode)
;; Subword mode
(subword-mode t)

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
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)
;; Doc View
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; ERC
(add-hook 'erc-mode-hook 'visual-line-mode)
;; TeX
(add-hook 'tex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'visual-line-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("1fab355c4c92964546ab511838e3f9f5437f4e68d9d1d073ab8e36e51b26ca6a" "7bf64a1839bf4dbc61395bd034c21204f652185d17084761a648251041b70233" "63b7b8a45190b2e7362a975067bd76b55ae548c00e9088d12b4133eb0525a604" default)))
 '(erc-server-reconnect-attempts t)
 '(fringe-mode 6 nil (fringe))
 '(global-auto-complete-mode t)
 '(linum-format " %7d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(package-selected-packages
   (quote
    (projectile-rails ess flycheck znc yaml-mode web-mode ruby-test-mode org markdown-mode magit haml-mode git-gutter fish-mode exec-path-from-shell evil coffee-mode auto-complete auctex ag)))
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(projectile-mode t nil (projectile))
 '(scroll-step 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#2222FF"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#22A0F0"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#22F0F0"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#22F0A0"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#22FF22"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#A0F022"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#F0A022"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FF2222"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#F022A0")))))
