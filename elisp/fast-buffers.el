;;; fast-buffers.el --- Faster buffer operations
;;;
;;; Commentary:
;;;
;;;   None of this is too radical, it simply assumes `current-buffer'
;;;   for interactve buffer functions
;;;
;;; Code:

(defun j3rn-last-buffer ()
  "Switch to the buffer that you were looking at before this one.

This differs from `previous-buffer' as it does not move backward
through history.  Instead, repeated invocations will simply
bounce you between your two most recently visited buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun j3rn-kill-buffer ()
  "Shortcut to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-c b") 'j3rn-last-buffer)
(global-set-key (kbd "C-c k") 'j3rn-kill-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(provide 'fast-buffers)
;;; fast-buffers.el ends here
