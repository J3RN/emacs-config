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
      '(dashboard page-break-lines counsel paradox go-mode dictionary neotree nim-mode projectile-ripgrep ripgrep rainbow-delimiters rust-mode rubocop org-bullets diff-hl monokai-theme org-projectile org-pomodoro smartparens which-key helm graphviz-dot-mode paredit projectile-rails ess flycheck znc yaml-mode web-mode ruby-test-mode org markdown-mode magit haml-mode git-gutter fish-mode exec-path-from-shell evil coffee-mode auto-complete auctex ag))

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
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Magit
(global-set-key (kbd "C-c m g") 'magit-status)
(global-set-key (kbd "C-c m a") 'magit-dispatch-popup)
(global-set-key (kbd "C-c m f") 'magit-file-popup)
;; Org mode
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
;; Neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)
;; Ivy
(global-set-key (kbd "C-c C-r") 'ivy-resume)
;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c u r") 'counsel-rg)
;; Swiper replaces isearch
(global-set-key (kbd "C-s") 'swiper)
;; Built-in
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
;; Faster buffers
(global-set-key (kbd "C-c b") (lambda ()
				(interactive)
				(switch-to-buffer (other-buffer (current-buffer) 1))))
(global-set-key (kbd "C-c k") (lambda ()
				(interactive)
				(kill-buffer (current-buffer))))

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
(global-linum-mode)
(global-subword-mode)
(show-paren-mode)
(global-auto-revert-mode)
;; Packages
(projectile-mode)
(projectile-rails-global-mode)
;; (helm-mode)
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
;; ANSI colorize compilation output
(defun colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in compilation output."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;; Customize command used for counsel-rg
(setq-default counsel-rg-base-command "rg -S -n --no-heading --color never %s .")

;;; Allow 'vm rails' for the projectile-rails vanilla rails command
(add-to-list 'safe-local-variable-values
	     '(projectile-rails-vanilla-command . "vm rails"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("390ede68e62beeb894d7b35c202153491774ad07a1e1e1745e679b5abd8c71be" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" default)))
 '(erc-server-reconnect-attempts t)
 '(exec-path-from-shell-shell-name "/bin/bash")
 '(inhibit-startup-screen t)
 '(linum-format " %4d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(org-agenda-files nil)
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (require
	    (quote projectile))
	   (puthash
	    (projectile-project-root)
	    "vm bundle exec rake test" projectile-test-cmd-map))
     (eval visual-line-mode t)
     (projectile-rails-vanilla-command . "vm rails"))))
 '(scroll-step 1)
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Menlo"))))
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
;; Tell Projectile to use Ivy for completion
(setq-default projectile-completion-system 'ivy)
;; Silver Searcher configuration
(setq-default ag-reuse-window 't)

;; Update packages with Paradox
(paradox-upgrade-packages)

;; Show startup screen
(dashboard-setup-startup-hook)

;; Example Ivy config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Start the server (so emacsclient can connect)
(server-start)

(provide 'init)
;;; init.el ends here