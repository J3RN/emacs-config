;;; haskell-config -- My Haskell configs
;;;
;;; Commentary:
;;;   I love Haskell
;;;
;;; Code:

(use-package haskell-mode
  :config
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-stylish-on-save t)
  :hook
  (haskell-mode . (lambda () (setq prettify-symbols-alist '(("<-" . 8592)
                                                       ("->" . 8594)
                                                       ("=>" . 8658)
                                                       ("/=" . 8800)
                                                       ("<=" . 8804)
                                                       (">=" . 8805)))))
  (haskell-mode . eglot-ensure))

(provide 'haskell-config)
;;; haskell-config.el ends here
