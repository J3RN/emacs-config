;;; j3rn-search --- A buffer search mechanism
;;;
;;; Commentary:
;;;
;;; Code:

(defun j3rn-search ()
  "Searches the whole buffer interactively."
  (interactive)
  (let ((new-point (save-excursion
                     (goto-char 0)
                     (isearch-forward-regexp))))
    (if new-point
        (goto-char new-point))))

(global-set-key (kbd "C-c s") 'j3rn-search)

(provide 'j3rn-search)
;;; j3rn-search.el ends here
