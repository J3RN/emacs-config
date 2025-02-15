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

;;; Tab bars
(load-theme 'adwaita nil t)
(custom-theme-set-faces
 'adwaita
 '(tab-bar ((((class color) (min-colors 89)) (:background "#E6E6E6"))))
 '(tab-bar-tab ((((class color) (min-colors 89)) (:background "#EDEDED" :foreground "#2E3436" :bold t :box (:line-width (10 . 5) :style flat-button)))))
 '(tab-bar-tab-inactive ((((class color) (min-colors 89)) (:background "#D9D9D9" :foreground "#2E3436" :bold t :box (:line-width (10 . 5) :style flat-button))))))
(load-theme 'wombat nil t)
(custom-theme-set-faces
 'wombat
 '(tab-bar ((((class color) (min-colors 89)) (:background "#303030"))))
 '(tab-bar-tab ((((class color) (min-colors 89)) (:background "#242424" :foreground "#f6f3e8" :weight bold :box (:line-width (10 . 5) :style flat-button)))))
 '(tab-bar-tab-inactive ((((class color) (min-colors 89)) (:background "#454545" :foreground "#ffffff" :weight bold :box (:line-width (10 . 5) :style flat-button))))))

;;; Reload themes
(seq-map (lambda (theme) (enable-theme theme)) custom-enabled-themes)

(provide 'tabs)
;;; tabs.el ends here
