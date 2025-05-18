;;; elm-config --- Elm packages and configuration
;;;
;;; Commentary:
;;;   Elm packages and configuration
;;;
;;; Code:

(use-package elm-mode
  :hook
  (elm-mode . eglot-ensure))

(provide 'elm-config)
;;; elm-config.el ends here
