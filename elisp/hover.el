;;; hover --- Highlight the thing under point
;;;
;;; Commentary:
;;;
;;;   This emulates the default(?) behavior in VS Code where having your cursor
;;;   over something will highlight all occurrences of said thing in your
;;;   buffer.
;;;
;;;   By default the "highlight" is actually bolding, but you can make it
;;;   whatever you want by customizing `hover-highlight-symbol-face'.
;;;
;;; Code:

(defface hover-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point.")

(defvar-local hover--hi-re nil
  "The regexp being highlighted.")

(defvar hover--idle-timer nil
  "Idle timer for the highligher.")

(defun hover--highlight-regexp ()
  "Return the regexp to highlight, if any."
  (when (memq (face-at-point)
	      '(nil
		 font-lock-function-name-face
		 font-lock-variable-name-face))
    (find-tag-default-as-symbol-regexp)))

(defun hover--reset-highlight ()
  "Highlight the regexp under point."
  (when hover-mode
    (when hover--hi-re
      (unhighlight-regexp hover--hi-re))
    (when-let ((re (hover--highlight-regexp)))
      (setq hover--hi-re re)
      (highlight-regexp re 'hover-highlight-symbol-face))))

(define-minor-mode hover-mode
  "Hovers the symbol under point when idle."
  :light " Hover"
  (if hover-mode
      (setq hover--idle-timer
	    (run-with-idle-timer 1 t #'hover--reset-highlight))
    (progn
      (when hover--hi-re
	(unhighlight-regexp hover--hi-re)
	(setq hover--hi-re nil))
      (cancel-timer hover--idle-timer))))

(provide 'hover)
;;; hover.el ends here
