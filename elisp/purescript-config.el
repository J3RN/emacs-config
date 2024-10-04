;;; purescript-config -- Purescript packages and config
;;;
;;; Commentary:
;;;   Just trying out Purescript.  This may be temporary.
;;;
;;; Code:

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode))

(provide 'purescript-config)
;;; purescript-config.el ends here
