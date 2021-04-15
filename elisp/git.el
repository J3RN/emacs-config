;;; git --- Git-related packages and configuration
;;; Commentary:
;;; Various Git-related packages and configuration
;;; Code:
(use-package forge)

(use-package git-gutter)

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

(use-package magit
  :bind
  ("C-c g s" . magit-status)
  ("C-c g f" . magit-file-dispatch)
  ("C-c g c" . magit-clone)
  :hook
  ;; This is a bit of a hack since the third argument that
  ;; `after-change-functions' provides to `ansi-color-apply-on-region' is the
  ;; length of the region changed, but the third parameter of
  ;; `ansi-color-apply-on-region' is a flag as to whether to preserve sequences.
  (magit-process-mode . (lambda () (add-hook 'after-change-functions 'ansi-color-apply-on-region nil 'local))))

(provide 'git)
;;; git.el ends here
