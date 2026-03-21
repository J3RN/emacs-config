;;; j3rn-nix --- Nix packges and configuration
;;; Commentary:
;;; Code:

(use-package nix-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (add-to-list 'treesit-language-source-alist '(nix "https://github.com/nix-community/tree-sitter-nix"))
  (when (fboundp 'treesit-install-language-grammar)
    (treesit-install-language-grammar 'nix)))

(provide 'j3rn-nix)
;;; j3rn-nix.el ends here
