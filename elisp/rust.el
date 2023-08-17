;;; rust --- Rust packages and configuration
;;; Commentary:
;;; Various Rust packages and configuration
;;; Code:
(use-package rust-mode
  :delight
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . (lambda () (setq-local indent-tabs-mode nil))))

(provide 'rust)
;;; rust.el ends here
