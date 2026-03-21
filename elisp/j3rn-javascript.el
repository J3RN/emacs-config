;;; j3rn-javascript --- JavaScript packages and configuration
;;;
;;; Commentary:
;;;   JavaScript packages and configuration
;;;
;;; Code:

(tempo-define-template "export default function"
		       '("export default function " p " {" n "}")
		       "edf"
		       "Inserts a JavaScript default exported function")

(provide 'j3rn-javascript)
;;; j3rn-javascript.el ends here
