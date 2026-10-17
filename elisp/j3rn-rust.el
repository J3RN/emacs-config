;;; j3rn-rust --- Rust packages and configuration  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Various Rust packages and configuration
;;;
;;; Code:
(use-package rust-mode
  :delight
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . (lambda () (setq-local indent-tabs-mode nil))))

(provide 'j3rn-rust)
;;; j3rn-rust.el ends here
