;;; j3rn-layout --- How I like my windows organized
;;; Commentary:
;;; Generally this follows a sort-of IDE sort of layout, where terminals and
;;; such are at the bottom, and some other things (e.g. Help) are at the top.
;;; Code:

(defvar j3rn-layout-top-windows
  '("\\*Help\\*")
  "A list of regular expressions that, if matched, will display the given buffer in the 'top'.")

(defvar j3rn-layout-bottom-windows
  '("\\*.?shell" "\\*elixir-test-output" "\\*exunit-compilation" "\\*Inf-Elixir" "\\*SQL" "\\*compilation\\*" "\\magit-process" "\\*haskell\\*")
  "A list of regular expressions that, if matched, will display the given buffer in the 'bottom'.")

(defun j3rn-layout-top-window-p (buffer action)
  "Predicate indicating whether BUFFER should be placed at the top of the frame.  ACTION."
  (seq-some (lambda (pattern) (eq 0 (string-match-p pattern buffer))) j3rn-layout-top-windows))

(defun j3rn-layout-bottom-window-p (buffer action)
  "Predicate indicating whether BUFFER should be placed at the bottom of the frame.  ACTION."
  (seq-some (lambda (pattern) (eq 0 (string-match-p pattern buffer))) j3rn-layout-bottom-windows))

(setq display-buffer-alist
      '((j3rn-layout-bottom-window-p display-buffer-in-side-window
			      (side . bottom)
			      (slot . -1)
			      (window-height . 0.5)
			      (inhibit-same-window . t))
	(j3rn-layout-top-window-p display-buffer-in-side-window
			   (side . top)
			   (slot . -1)
			   (window-height . 0.5)
			   (inhibit-same-window . t))))

(provide 'j3rn-layout)
;;; j3rn-layout.el ends here
