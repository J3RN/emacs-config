;;; javascript-config --- JavaScript packages and configuration
;;;
;;; Commentary:
;;;   JavaScript packages and configuration
;;;
;;; Code:

(tempo-define-template "export default function"
		       '("export default function " p " {" n "}")
		       "edf"
		       "Inserts a JavaScript default exported function")

(provide 'javascript-config)
;;; javascript-config.el ends here
