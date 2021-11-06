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
  (magit-process-mode . (lambda () (add-hook 'after-change-functions (lambda (beg end diff) (ansi-color-apply-on-region beg end)) nil 'local))))

(provide 'git)
;;; git.el ends here
