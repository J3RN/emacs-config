;;; j3rn-git --- Git-related packages and configuration
;;;
;;; Commentary:
;;;   Various Git-related packages and configuration
;;;
;;; Code:

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package forge
  :ensure nil)

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

(use-package magit
  :ensure nil
  :bind
  ("C-c g s" . magit-status)
  ("C-c g f" . magit-file-dispatch)
  ("C-c g c" . magit-clone)
  :config
  (unbind-key "C-<tab>" magit-section-mode-map)
  (require 'magit-extras)
  :hook
  (magit-process-mode . (lambda () (add-hook 'after-change-functions (lambda (beg end diff) (ansi-color-apply-on-region beg end)) nil 'local)))
  (magit-process-mode . visual-line-mode))

(provide 'j3rn-git)
;;; j3rn-git.el ends here
