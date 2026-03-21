;;; j3rn-dhall --- Packages and config for Dhall
;;; Commentary:
;;; Code:

(use-package dhall-mode
  :hook
  (dhall-mode . dhall-format-on-save-mode))

(provide 'j3rn-dhall)
;;; j3rn-dhall.el ends here
