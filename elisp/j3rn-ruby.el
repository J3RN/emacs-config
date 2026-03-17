;;; j3rn-ruby --- Ruby-related packages and configuration
;;;
;;; Commentary:
;;;   Various Ruby packages and configuration
;;;
;;; Code:

;; Bundler package is dead, evidently
;; (use-package bundler
;;   :bind (("C-c u i" . bundle-install)
;; 	 ("C-c u o" . bundle-open)
;; 	 ("C-c u e" . bundle-exec)
;; 	 ("C-c u u" . bundle-update)))

(use-package enh-ruby-mode
  :delight
  :config
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
  :bind
  ("C-c r '" . ruby-toggle-string-quotes)
  ("C-c r {" . enh-ruby-toggle-block))

(use-package inf-ruby)

(use-package rake)

(use-package rspec-mode
  :delight
  :hook ((rspec-compilation-mode . visual-line-mode)))

(use-package rubocop
  :bind (("C-c c c p" . rubocop-check-project)
	 ("C-c c c f" . rubocop-check-current-file)
	 ("C-c c a p" . rubocop-autocorrect-project)
	 ("C-c c a f" . rubocop-autocorrect-current-file)))

(use-package ruby-end
  :delight)

(use-package ruby-hash-syntax
  :config
  (define-key enh-ruby-mode-map (kbd "C-c h") 'ruby-hash-syntax-toggle))

(use-package ruby-test-mode)

(provide 'j3rn-ruby)
;;; j3rn-ruby.el ends here
