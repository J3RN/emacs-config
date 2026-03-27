;;; j3rn-gleam --- Gleam packages and configuration
;;;
;;; Commentary:
;;;
;;; Code:

(use-package gleam-ts-mode
  :ensure nil
  :mode (rx ".gleam" eos)
  :config
  (add-to-list 'eglot-server-programs '(gleam-ts-mode "gleam" "lsp"))
  :hook (gleam-ts-mode . eglot-ensure))

(provide 'j3rn-gleam)
;;; j3rn-gleam.el ends here
