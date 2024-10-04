;;; git-config --- Git-related packages and configuration
;;;
;;; Commentary:
;;;   Various Git-related packages and configuration
;;;
;;; Code:

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package forge)

(use-package git-gutter)

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

(use-package magit
  :bind
  ("C-c g s" . magit-status)
  ("C-c g f" . magit-file-dispatch)
  ("C-c g c" . magit-clone)
  :config
  (unbind-key "C-<tab>" magit-section-mode-map)
  :hook
  (magit-process-mode . (lambda () (add-hook 'after-change-functions (lambda (beg end diff) (ansi-color-apply-on-region beg end)) nil 'local)))
  (magit-process-mode . visual-line-mode))

(provide 'git-config)
;;; git-config.el ends here
