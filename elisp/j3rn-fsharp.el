;;; j3rn-fsharp -- Tools and config for writing F#
;;; Commentary:
;;; Code:

(use-package eglot-fsharp)

(use-package fsharp-mode
  :hook (fsharp-mode . eglot-ensure))

(provide 'j3rn-fsharp)
;;; j3rn-fsharp.el ends here
