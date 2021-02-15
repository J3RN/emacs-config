;;; elisp --- Summary
;;; Commentary:
;;; Code:

(use-package flycheck-package)

(use-package lispy)

(use-package package-lint)

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :delight
  :bind (("C-c M-s" . paredit-splice-sexp)
	 ("C-S-<right>" . paredit-forward-slurp-sexp)
	 ("C-S-<left>" . paredit-forward-barf-sexp)
	 ("C-M-<right>" . paredit-backward-barf-sexp)
	 ("C-M-<left>" . paredit-backward-slurp-sexp)))

(provide 'elisp)
;;; elisp.el ends here
