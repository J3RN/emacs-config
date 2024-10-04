;;; gleam-config --- Gleam packages and configuration
;;;
;;; Commentary:
;;;
;;; Code:

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos)
  :config
  (add-to-list 'eglot-server-programs '(gleam-ts-mode "gleam" "lsp"))
  :hook (gleam-ts-mode . eglot-ensure))

(provide 'gleam-config)
;;; gleam-config.el ends here
