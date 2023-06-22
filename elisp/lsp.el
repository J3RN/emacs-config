;;; lsp --- Language Server (Protocol) integration
;;; Commentary:
;;; Code:
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :bind ("C-c l w s" . lsp-ivy-workspace-symbol))

(use-package lsp-mode
  :delight
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-restart 'interactive)
  (setq lsp-file-watch-threshold 50000)
  (setq lsp-elixir-suggest-specs nil)
  (setq lsp-log-io t)
  :hook
  (web-mode . lsp)
  (lsp-mode . lsp-headerline-breadcrumb-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-delay 0.5)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(provide 'lsp)
;;; lsp.el ends here
