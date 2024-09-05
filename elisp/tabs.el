;;; tabs.el -- Tab configuration
;;; Commentary:
;;; Code:

(setq tab-bar-tab-name-function (lambda () (let ((tname (projectile-project-name)))
					(if (not (equal "-" tname))
					    tname
					  (tab-bar-tab-name-current)))))

(setq tab-bar-new-tab-choice 'dashboard-open)
(define-icon tab-bar-new nil
  `((image ,(locate-user-emacs-file (file-name-concat "images" "add.svg"))
	   :ascent center)
    (text " + "))
  "Icon for creating a new tab."
  :scale 0.75
  ;; The version here is a bit silly
  :version "29.1"
  :help-echo "New tab")

(setq tab-bar-close-last-tab-choice 'delete-frame)
(define-icon tab-bar-close nil
  `((image ,(locate-user-emacs-file (file-name-concat "images" "close.svg"))
	   :ascent center)
    (text " x "))
  "Icon for closing the clicked tab."
  :scale 0.75
  ;; The version here is a bit silly
  :version "29.1"
  :help-echo "Click to close tab")

(tab-bar-mode 1)

(provide 'tabs)
;;; tabs.el ends here
