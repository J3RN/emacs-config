;;; nix-config --- Nix packges and configuration
;;; Commentary:
;;; Code:

(use-package nix-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (add-to-list 'treesit-language-source-alist '(nix "https://github.com/nix-community/tree-sitter-nix"))
  (treesit-install-language-grammar 'nix))

(provide 'nix-config)
;;; nix-config.el ends here
