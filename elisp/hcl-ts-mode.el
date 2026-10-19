;;; hcl-ts-mode.el --- Major mode for HCL -*- lexical-binding: t -*-

;; Copyright © 2026 Jonathan Arnett <j3rn@j3rn.com>
;; Authors: Jonathan Arnett <j3rn@j3rn.com>
;;
;; Keywords: languages hcl
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides syntax highlighting, indentation, and code navigation
;; features for HCL (Hashicorp Configuration Language).

;;; Code:

(require 'prog-mode)
(require 'treesit)


;;; Customization

(defgroup hcl-ts nil
  "Major mode for editing HCL."
  :prefix "hcl-ts-"
  :group 'languages)

(defcustom hcl-ts-indent-offset 2
  "Offset used to indent HCL code."
  :type 'integer
  :safe 'integerp
  :group 'hcl-ts)


;;; Tree-sitter font locking

(defface hcl-ts-constructor-face
  '((t (:inherit font-lock-type-face)))
  "Font used for highlighting HCL type constructors.")

(defface hcl-ts-module-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font used for highlighting HCL modules.")

(defvar hcl-ts--grammar-revision
  (if (and (treesit-available-p) (< (treesit-library-abi-version) 15))
      "v1.0.0"
    "main")
  "Treesitter grammar (hcl-lang/tree-sitter-hcl repo) revision.")

(defvar hcl-ts--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'hcl
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'hcl
   '([
      (string_lit)
      (quoted_template)
      (heredoc_template)
      ] @font-lock-string-face)

   ;;    :feature 'number
   ;;    :language 'hcl
   ;;    '((integer) @font-lock-number-face
   ;;      (float) @font-lock-number-face)

   ;;    :feature 'function-name
   ;;    :language 'hcl
   ;;    '((unqualified_import (identifier) @font-lock-function-name-face)
   ;;      (function
   ;;       name: (identifier) @font-lock-function-name-face)
   ;;      (external_function
   ;;       name: (identifier) @font-lock-function-name-face))

   ;;    :feature 'function-use
   ;;    :language 'hcl
   ;;    '((function_call function: (identifier) @font-lock-function-call-face)
   ;;      (function_call
   ;;       function: (field_access record: (identifier) field: (label) @font-lock-function-call-face)))

   :feature 'variable-name
   :language 'hcl
   '((identifier) @font-lock-variable-name-face)

   ;;    :feature 'constant-name
   ;;    :language 'hcl
   ;;    :override t
   ;;    '((constant
   ;;       name: (identifier) @font-lock-constant-face))

   ;;    :feature 'keyword
   ;;    :language 'hcl
   ;;    (let* ((basic-keywords
   ;;            [
   ;;             (visibility_modifier)
   ;;             (opacity_modifier)
   ;;             "as"
   ;;             "assert"
   ;;             "case"
   ;;             "const"
   ;;             ;; Deprecated
   ;;             "external"
   ;;             "fn"
   ;;             "if"
   ;;             "import"
   ;;             "let"
   ;;             "panic"
   ;;             "todo"
   ;;             "type"
   ;;             "use"
   ;;             ])
   ;;           (keywords (if (hcl-ts--grammar-supports-echo-keyword-p)
   ;;                         (vconcat basic-keywords ["echo"])
   ;;                       basic-keywords)))
   ;;      (list keywords '@font-lock-keyword-face))

   ;;    :feature 'operator
   ;;    :language 'hcl
   ;;    '((binary_expression operator: _ @font-lock-operator-face)
   ;;      (boolean_negation "!" @operator)
   ;;      (integer_negation "-" @operator))

   ;;    :feature 'annotation
   ;;    :language 'hcl
   ;;    :override t
   ;;    '((attribute
   ;;       "@" @font-lock-preprocessor-face
   ;;       name: (identifier) @font-lock-preprocessor-face))

   ;;    :feature 'documentation
   ;;    :language 'hcl
   ;;    '((module_comment) @font-lock-doc-face
   ;;      (statement_comment) @font-lock-doc-face)

   ;;    :feature 'module
   ;;    :language 'hcl
   ;;    :override t
   ;;    '((module) @hcl-ts-module-face
   ;;      (import alias: (identifier) @hcl-ts-module-face)
   ;;      (remote_type_identifier
   ;;       module: (identifier) @hcl-ts-module-face))

   ;;    :feature 'builtin
   ;;    :language 'hcl
   ;;    :override t
   ;;    '((bit_string_segment_option) @font-lock-builtin-face)

   :feature 'bracket
   :language 'hcl
   '([
      "("
      ")"
      "["
      "]"
      "{"
      "}"
      ] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'hcl
   '([
      "="
      ] @font-lock-delimiter-face)))


(defvar hcl-ts--indent-rules
  (let ((offset 'hcl-ts-indent-offset))
    `((hcl
       ((parent-is "^body$") parent-bol ,offset)))))


;;; Public functions

;;;###autoload
(defun hcl-ts-install-grammar ()
  "Install the HCL tree-sitter grammar."
  (interactive)
  (if (and (treesit-available-p) (boundp 'treesit-language-source-alist))
      (let* ((language-source (list "https://github.com/tree-sitter-grammars/tree-sitter-hcl" hcl-ts--grammar-revision))
             (treesit-language-source-alist
              (cons
               (cons 'hcl language-source)
               treesit-language-source-alist)))
        (treesit-install-language-grammar 'hcl))
    (display-warning 'treesit "Emacs' treesit package does not appear to be available")))


;;; Major mode definition
;;;###autoload
(define-derived-mode hcl-ts-mode prog-mode "HCL"
  "Major mode for editing HCL.

\\<hcl-ts-mode-map>"
  :group 'hcl-ts

  (cond
   ((treesit-ready-p 'hcl)
    (treesit-parser-create 'hcl)

    (setq-local treesit-font-lock-settings hcl-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string variable-name)
                  ()
                  (bracket delimiter)
                  ()))

    (setq-local treesit-simple-indent-rules hcl-ts--indent-rules)

    (setq-local treesit-simple-imenu-settings '())
                ;; '(("Public Functions"           "^function$"       (lambda (fun) (and (hcl-ts--public fun) (hcl-ts--internal-fun fun))) hcl-ts--function-name)
                ;;   ("Public Types"               "^type_definition" (lambda (type) (and (hcl-ts--public type) (hcl-ts--transparent type))) hcl-ts--type-name)
                ;;   ("Public Type Alias"          "^type_alias"      hcl-ts--public  hcl-ts--type-name)
                ;;   ("Public Opaque Types"        "^type_definition" (lambda (type) (and (hcl-ts--public type) (hcl-ts--opaque type))) hcl-ts--type-name)
                ;;   ("Public Constants"           "^constant$"       hcl-ts--public  hcl-ts--constant-name)
                ;;   ("Public External Functions"  "^function$"       (lambda (fun) (and (hcl-ts--public fun) (hcl-ts--external-fun fun))) hcl-ts--function-name)
                ;;   ("Public External Types"      "^external_type"   hcl-ts--public  hcl-ts--type-name)
                ;;   ("Private Functions"          "^function$"       hcl-ts--private hcl-ts--function-name)
                ;;   ("Private Types"              "^type_definition" hcl-ts--private hcl-ts--type-name)
                ;;   ("Private Type Alias"         "^type_alias"      hcl-ts--private hcl-ts--type-name)
                ;;   ("Private Constants"          "^constant$"       hcl-ts--private hcl-ts--constant-name)
                ;;   ("Private External Functions" "^todo"            (lambda (fun) (and (hcl-ts--private fun) (hcl-ts--external-fun fun))) hcl-ts--function-name)
                ;;   ("Private External Types"     "^external_type"   hcl-ts--private hcl-ts--type-name)))

    (setq-local comment-start "# ")
    (setq-local comment-start-skip (rx "#" (* (syntax whitespace))))
    (setq-local comment-end "")
    (setq-local comment-end-skip
                (rx (* (syntax whitespace))
                    (group (or (syntax comment-end) "\n"))))

    (treesit-major-mode-setup))
   (t
    (message "Cannot load tree-sitter-hcl.  Try running `hcl-ts-install-grammar' and report a bug if the issue reoccurs."))))

(provide 'hcl-ts-mode)
;;; hcl-ts-mode.el ends here
