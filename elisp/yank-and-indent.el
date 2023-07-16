;;; yank-and-indent --- Indent code when "yanking" (pasting) it
;;;
;;; Commentary:
;;; Seems like this would be built-in, but so far as I'm aware, it's not.
;;; Use a prefix command (e.g. C-u) before invoking yank to disable.
;;;
;;; Code:

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (if (and (not current-prefix-arg)
		    (derived-mode-p 'prog-mode))
	       (let ((mark-even-if-inactive transient-mark-mode))
		 (indent-region (region-beginning) (region-end) nil))))))

(provide 'yank-and-indent)
;;; yank-and-indent.el ends here
