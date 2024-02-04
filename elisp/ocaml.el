;;; ocaml --- My OCaml config
;;; Commentary:
;;; Code:
(use-package tuareg)
(use-package reason-mode)

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (ignore-errors (require 'dune))
    (ignore-errors (require 'dune-flymake))
    (ignore-errors (require 'dune-watch))
    (ignore-errors (require 'ocamlformat))
    (ignore-errors (require 'ocp-indent))
    ;; Register Merlin
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (add-hook 'reason-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(provide 'ocaml)
;;; ocaml.el ends here
