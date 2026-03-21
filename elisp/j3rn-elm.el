;;; j3rn-elm --- Elm packages and configuration
;;;
;;; Commentary:
;;;   Elm packages and configuration
;;;
;;; Code:

(use-package elm-mode
  :hook
  (elm-mode . eglot-ensure))

(provide 'j3rn-elm)
;;; j3rn-elm.el ends here
