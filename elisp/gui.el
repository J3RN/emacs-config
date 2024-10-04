;;; gui --- Graphical UI customizations
;;;
;;; Commentary:
;;;   Generally seeks to make the UI a bit more minimal than the default
;;;
;;; Code:

(use-package all-the-icons
  :config (all-the-icons-install-fonts t))

;; Disable toolbar
(tool-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Bind normal mac fullscreenifying shortcut
(if (string-equal "darwin" system-type)
    (progn
      (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
      (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)))

(provide 'gui)
;;; gui.el ends here
