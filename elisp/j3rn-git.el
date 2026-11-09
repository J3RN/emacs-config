;;; j3rn-git --- Git-related packages and configuration  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Various Git-related packages and configuration
;;;
;;; Code:

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

(use-package magit
  :bind
  ("C-c g s" . magit-status)
  ("C-c g f" . magit-file-dispatch)
  ("C-c g c" . magit-clone)
  :config
  (setq magit-branch-prefer-remote-upstream t)
  (unbind-key "C-<tab>" magit-section-mode-map)
  (require 'magit-extras)
  :hook
  (magit-process-mode . (lambda () (add-hook 'after-change-functions (lambda (beg end diff) (ansi-color-apply-on-region beg end)) nil 'local)))
  (magit-process-mode . visual-line-mode))

;;; Relies on magit
(defun j3rn-worktree-branch ()
  "Create a new branch and worktree where the worktree's directory is the branch name."
  (interactive)
  (pcase-let* ((`(,branch ,start-point) (magit-branch-read-args
                                         "In new worktree; checkout new branch"
                                         (magit-get-push-branch (magit-main-branch))))
               (dir (concat
                     (file-name-parent-directory (magit-toplevel))
                     branch)))
    (magit-worktree-branch dir branch start-point)))

(provide 'j3rn-git)
;;; j3rn-git.el ends here
