;;; tabs.el -- Tab configuration
;;;
;;; Commentary:
;;;   Customize tabs to look more modern
;;;
;;; Code:

;; Label a tab with the projectile-project-name, if available
(setq tab-bar-tab-name-function (lambda () (let ((tname (projectile-project-name)))
					(if (not (equal "-" tname))
					    tname
					  (tab-bar-tab-name-current)))))

;; Go to Dashboard when opening a new tab
(setq tab-bar-new-tab-choice 'dashboard-open)

;; Use a custom SVG for the new tab icon
(define-icon tab-bar-new nil
  `((image ,(locate-user-emacs-file (file-name-concat "images" "add.svg"))
	   :ascent center)
    (text " + "))
  "Icon for creating a new tab."
  :scale 0.75
  ;; The version here is a bit silly
  :version "29.1"
  :help-echo "New tab")

;; Use a custom SVG for the close tab icon
(define-icon tab-bar-close nil
  `((image ,(locate-user-emacs-file (file-name-concat "images" "close.svg"))
	   :ascent center)
    (text " x "))
  "Icon for closing the clicked tab."
  :scale 0.75
  ;; The version here is a bit silly
  :version "29.1"
  :help-echo "Click to close tab")

;; When closing the last tab in a frame, attempt to delete that frame
(setq tab-bar-close-last-tab-choice 'delete-frame)

;; Enable tabs
(tab-bar-mode 1)

(provide 'tabs)
;;; tabs.el ends here
