;;; haskell -- My Haskell configs

;;; Commentary:
;;; Code:

(use-package haskell-mode
  :config
  (setq haskell-process-type 'stack-ghci)
  :hook
  (haskell-mode . (lambda () (setq prettify-symbols-alist '(("<-" . 8592)
							    ("->" . 8594)
							    ("=>" . 8658)
							    ("/=" . 8800)
							    ("<=" . 8804)
							    (">=" . 8805))))))

(provide 'haskell)
;;; haskell.el ends here
