;;; rust --- Rust packages and configuration
;;; Commentary:
;;; Various Rust packages and configuration
;;; Code:
(use-package rust-mode
  :delight
  :hook
  (rust-mode . lsp))

(provide 'rust)
;;; rust.el ends here
