;;; dhall-config --- Packages and config for Dhall
;;; Commentary:
;;; Code:

(use-package dhall-mode
  :hook
  (dhall-mode . dhall-format-on-save-mode))

(provide 'dhall-config)
;;; dhall-config.el ends here
