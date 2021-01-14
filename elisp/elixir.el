;;; package --- Summary
;;; Commentary:
;;; Various Elixir packages and configuration
;;; Code:

(use-package elixir-mode
  :delight
  :hook
  (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil 'local)))
  (elixir-mode . (lambda () (setq indent-tabs-mode nil)))
  (elixir-mode . lsp))

(defun j3rn-elixir-relative-path ()
  "Return the path after lib/."
  (car (last (split-string (pwd) "lib/"))))

(defun j3rn-elixir-module-name ()
  "Determine a reasonable Elixir module name for the current buffer."
  (string-join (append
		(mapcar 's-upper-camel-case (split-string
					     (string-remove-suffix
					      "/"
					      (j3rn-elixir-relative-path)) "/"))
		(list (s-upper-camel-case (file-name-sans-extension (buffer-name)))))
	       "."))

(tempo-define-template "exmodule"
		       '("defmodule " p (j3rn-elixir-module-name) " do" n> p n "end" >)
		       "em"
		       "Inserts a new Elixir module")
(tempo-define-template "exfun"
		       '("@spec " (p "name: " funname) "(" (p "argument specs: ") ") :: " (p "return type: ") n > "def " (s funname) "(" (p "args: ") ") do" n > p n "end" >)
		       "ed"
		       "Inserts an Elixir function declaration")
(tempo-define-template "exdefp"
		       '("defp " (p "name: ") "(" (p "args: ") ") do" n > p n "end" >)
		       "edp"
		       "Inserts a private Elixir function declaration")
(tempo-define-template "exdescribe"
		       '("describe \"" (p "what? ") "\" do" n> p n "end" >)
		       "ede"
		       "Inserts a new ExUnit describe block")
(tempo-define-template "extest"
		       '("test \"" (p "description: ") "\", %{" (p "context: ") "} do" n > p n "end" >)
		       "et"
		       "Inserts a standard ExUnit test declaration")
(tempo-define-template "exsetup"
		       '("setup context do" n > p n "end" >)
		       "es"
		       "Inserts a standard ExUnit setup block")
(tempo-define-template "exfn"
		       '("fn " (p "arg: ") " ->" n > p > n "end" >)
		       "fn"
		       "Inserts an anonymous Elixir function")

(provide 'elixir)
;;; elixir.el ends here
