;;; j3rn-relative-modeline -- Show buffer filename relative to project root (if any)
;;; Commentary:
;;; Code:

(defvar-local j3rn--buffer-id nil
  "Cached project-relative buffer file name.")

(defun j3rn-project-relative-buffer-id ()
  (if j3rn--buffer-id
      j3rn--buffer-id
    (setq j3rn--buffer-id
          (if-let* ((file (buffer-file-name))
                    (proj (project-current))
                    (root (project-root proj)))
              (propertized-buffer-identification (file-relative-name file root))
            (propertized-buffer-identification "%12b")))))

(setq-default mode-line-buffer-identification '(:eval (j3rn-project-relative-buffer-id)))

(provide 'j3rn-relative-modeline)
;;; j3rn-relative-modeline.el ends here
