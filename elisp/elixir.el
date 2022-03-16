;;; elixir --- Elixir packages and configuration
;;; Commentary:
;;; Various Elixir packages and configuration
;;; Code:

(use-package elixir-mode
  :delight
  :hook
  (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil 'local)))
  (elixir-mode . (lambda () (setq indent-tabs-mode nil)))
  (elixir-mode . lsp))

(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer))
  :hook
  (inf-elixir-mode . abbrev-mode))

(use-package elixir-test
  :load-path "~/Code/J3RN/elixir-test-mode/master"
  :bind (:map elixir-test-mode-map
              ("C-c e" . elixir-test-command-map))
  :hook (elixir-mode . elixir-test-mode))

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
(tempo-define-template "exprotocol"
		       '("defprotocol " p (j3rn-elixir-module-name) " do" n> p n "end" >)
		       "ep"
		       "Inserts a new Elixir protocol")
(tempo-define-template "exfun"
		       '("@doc \"\"\"" n > (p "doc: " doc) n > "\"\"\"" n > "@spec " (p "name: " funname) "(" (p "argument specs: ") ") :: " (p "return type: ") n > "def " (s funname) "(" (p "args: ") ") do" n > p n "end" >)
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
