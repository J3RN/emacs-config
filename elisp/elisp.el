;;; elisp --- Packages and configuration for Emacs Lisp
;;; Commentary:
;;; Various packages and configurations thereof for authoring Emacs Lisp
;;; packages.
;;; Code:

(use-package flycheck-package
  :config (flycheck-package-setup))

(use-package lispy)

(use-package package-lint)

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :delight
  :bind (("C-c M-s" . paredit-splice-sexp)))

(provide 'elisp)
;;; elisp.el ends here
