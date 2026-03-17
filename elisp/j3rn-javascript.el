;;; j3rn-javascript --- JavaScript packages and configuration
;;;
;;; Commentary:
;;;   JavaScript packages and configuration
;;;
;;; Code:

(add-hook 'js-mode-hook (lambda () (setq prettify-symbols-alist '(("->" . 8594)
                                                                  ("=>" . 8658)
                                                                  ("<=" . 8804)
                                                                  (">=" . 8805)
                                                                  ("||=" . 9568)))))

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(tempo-define-template "export default function"
		       '("export default function " p " {" n "}")
		       "edf"
		       "Inserts a JavaScript default exported function")

(provide 'j3rn-javascript)
;;; j3rn-javascript.el ends here
