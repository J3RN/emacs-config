;;; fsharp-config -- Tools and config for writing F#
;;; Commentary:
;;; Code:

(use-package eglot-fsharp)

(use-package fsharp-mode
  :hook (fsharp-mode . eglot-ensure))
