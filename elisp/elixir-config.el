;;; elixir-config --- Elixir packages and configuration
;;;
;;; Commentary:
;;;   Various Elixir packages and configuration
;;;
;;; Code:

(defun elixir-expand-keyword ()
  "Expands a single word like `foo' into an association like `foo: foo'."
  (interactive)
  (if-let ((symbol-to-dup (symbol-at-point))
	   (to-dup (symbol-name symbol-to-dup)))
      (insert ": " to-dup)
    (message "No symbol at point")))

(add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) . ,(eglot-alternatives '("expert" "expert_linux_amd64" "lexical" "nextls" "elixir-ls"))))

(use-package elixir-mode
  :delight
  :bind (:map elixir-mode-map ("C-c e :" . elixir-expand-keyword))
  :hook
  (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil 'local)))
  (elixir-mode . (lambda () (setq indent-tabs-mode nil)))
  (elixir-mode . (lambda () (setq prettify-symbols-alist '(("<-" . 8592)
						      ("->" . 8594)
						      ("=>" . 8658)
						      ("!=" . 8800)
						      ("<=" . 8804)
						      (">=" . 8805)
						      ("|>" . 9655)))))
  (elixir-mode . eglot-ensure))

(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module))
  :hook
  (inf-elixir-mode . abbrev-mode))

(use-package elixir-test
  :vc (:url "https://github.com/J3RN/elixir-test-mode")
  :bind (:map elixir-test-mode-map
              ("C-c e" . elixir-test-command-map))
  :hook
  (elixir-mode . elixir-test-mode)
  (elixir-ts-mode . elixir-test-mode))

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

(define-skeleton elixir-do-end
  "Automatically insert `end' after a `do'." nil
  _ > "\nend" >)

(defun elixir-maybe-do-end ()
  "Insert an `end' to your `do' if line is determined to need it."
  (if (looking-back " do" 3)
      (elixir-do-end)))

(define-abbrev elixir-mode-abbrev-table "do" "do" 'elixir-maybe-do-end)

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
(tempo-define-template "exfunsmol"
		       '("def " (p "name: " funname) "(" (p "args: ") ") do" n > p n "end" >)
		       "eds"
		       "Inserts an minimal Elixir function declaration")
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
(tempo-define-template "exdoc"
		       '("\"\"\"" n> p n> "\"\"\"")
		       "edo"
		       "Inserts an Elixir HEREDOC-style string")

(add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir" nil nil))

(provide 'elixir-config)
;;; elixir-config.el ends here
