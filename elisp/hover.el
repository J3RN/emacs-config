;;; hover --- Highlight the thing under point
;;;
;;; Commentary:
;;;
;;; Still very much in need of improvement and customization (and a *mode*), but
;;; this emulates the default(?) behavior in VS Code where having your cursor
;;; over something will highlight all occurrences of said thing in your buffer.
;;;
;;; Code:

(defvar hover--hi-re nil
  "The regexp being highlighted.")

(defvar hover--idle-timer nil
  "Idle timer for the highligher.")

(defun hover--reset-highlight ()
  "Highlight the regexp under point."
  (when hover--hi-re
    (unhighlight-regexp hover--hi-re))
  (when-let ((re (find-tag-default-as-symbol-regexp)))
    (setq hover--hi-re re)
    (highlight-regexp re)))

(setq hover--idle-timer
      (run-with-idle-timer 1 t #'hover--reset-highlight))

(provide 'hover)
;;; hover.el ends here
