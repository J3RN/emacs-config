;;; csharp-config --- Tools for working with C# and .NET
;;;
;;; Commentary:
;;;
;;;   I can't believe it either
;;;
;;; Code:

(defun csharpier ()
  "Format the current buffer's file with csharpier."
  (interactive)
  (when-let ((filename (buffer-file-name)))
      (shell-command (concat "dotnet-csharpier " filename))))

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
  :hook
  (csharp-ts-mode . (lambda () (add-hook 'before-save-hook 'csharpier nil 'local)))
  (csharp-ts-mode . eglot-ensure)
  (csharp-mode . (lambda () (add-hook 'before-save-hook 'csharpier nil 'local)))
  (csharp-mode . eglot-ensure))

(provide 'csharp-config)
;;; csharp-config.el ends here
