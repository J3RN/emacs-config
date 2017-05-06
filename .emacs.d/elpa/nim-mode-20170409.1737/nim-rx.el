;;; nim-rx.el --- -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'nim-vars)

(eval-and-compile
  (defvar nim-rx-constituents
    (let* ((constituents1
            (cl-loop for (sym . kwd) in `((dedenter          . ("elif" "else" "of" "except" "finally"))
                                          (defun             . ("proc" "method" "converter" "iterator" "template" "macro"))
                                          (block-start-defun . ("proc" "method" "converter" "iterator"
                                                                "template" "macro"
                                                                "if" "elif" "else" "when" "while" "for" "case" "of"
                                                                "try" "except" "finally"
                                                                "with" "block"
                                                                "enum" "tuple" "object")))
                     collect (cons sym (apply `((lambda () (rx symbol-start (or ,@kwd) symbol-end)))))))
           (constituents2 `((decl-block . ,(rx symbol-start
                                               (or "type" "const" "var" "let" "import")
                                               symbol-end
                                               (* space)
                                               (or "#" eol)))
                            (symbol-name          . ,(rx (any letter ?_ ?–) (* (any word ?_ ?–))))
                            (hex-lit . ,(rx "0" (or "x" "X") xdigit (0+ (or xdigit "_"))))
                            (dec-lit . ,(rx digit (0+ (or digit "_"))))
                            (oct-lit . ,(rx "0" (in "ocC") (in "0-7") (0+ (in "0-7_"))))
                            (bin-lit . ,(rx "0" (in "bB") (in "01") (0+ (in "01_"))))

                            (exponent
                             . ,(rx (group (in "eE") (? (or "+" "-")) digit (0+ (or "_" digit)))))
                            (open-paren           . ,(rx (or "{" "[" "(")))
                            (close-paren          . ,(rx (or "}" "]" ")")))
                            ;; FIXME: Use regexp-opt.
                            (operator             . ,(rx (or (1+ (in "-=+*/<>@$~&%|!?^.:\\"))
                                                             (and
                                                              symbol-start
                                                              (or
                                                               "and" "or" "not" "xor" "shl"
                                                               "shr" "div" "mod" "in" "notin" "is"
                                                               "isnot")
                                                              symbol-end))))
                            (string-delimiter . ,(rx (and
                                                      ;; Match even number of backslashes.
                                                      (or (not (any ?\\ ?\")) point
                                                          ;; Quotes might be preceded by a escaped quote.
                                                          (and (or (not (any ?\\)) point) ?\\
                                                               (* ?\\ ?\\) (any ?\")))
                                                      (* ?\\ ?\\)
                                                      ;; Match single or triple quotes of any kind.
                                                      (group (or  "\"" "\"\"\"")))))
                            (string . ,(rx
                                        (minimal-match
                                         (group (syntax string-delimiter)
                                                (0+ (or any "\n"))
                                                (syntax string-delimiter)))))
                            (character-delimiter
                             ;; Implemented with
                             ;; http://nim-lang.org/docs/manual.html#lexical-analysis-character-literals
                             . ,(rx
                                 (group "'")
                                 (or
                                  ;; escaped characters
                                  (and ?\\ (or (in "a-c" "e" "f" "l" "r" "t" "v"
                                                   "\\" "\"" "'")
                                               (1+ digit)
                                               (and "x" (regex "[a-fA-F0-9]\\{2,2\\}"))))
                                  ;; One byte characters(except single quote and control characters)
                                  (eval (cons 'in (list (concat (char-to-string 32) "-" (char-to-string (1- ?\')))
                                                        (concat (char-to-string (1+ ?\')) "-" (char-to-string 126))))))
                                 (group "'"))))))
      (append constituents1 constituents2))
    "Additional Nim specific sexps for `nim-rx'.")

  (defmacro nim-rx (&rest regexps)
    "Nim mode specialized rx macro.
This variant of `rx' supports common nim named REGEXPS."
    (let ((rx-constituents (append nim-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t)))))

  ;; based on nim manual
  (add-to-list 'nim-rx-constituents
               ;; note rx.el already has ‘letter’
               (cons 'nim-letter (rx (in "a-zA-Z-ÿ"))))

  (add-to-list 'nim-rx-constituents
               (cons 'identifier (nim-rx nim-letter
                                         (0+ (or "_" nim-letter digit)))))

  (add-to-list
   'nim-rx-constituents
   (cons 'quoted-chars
         (nim-rx
          (minimal-match
           (and "`"
                (1+ (or
                     nim-letter digit "_"
                     "^" "*" "[" "]" "!" "$" "%" "&" "+" "-"
                     "." "/" "<" "=" ">" "?" "@" "|" "~"))
                "`")))))

  (add-to-list 'nim-rx-constituents
               (cons 'comment
                     (rx (1+ (syntax comment-start))
                         (0+ (or (in " " word) nonl) (syntax comment-end)))))

  ;; Numbers
  (add-to-list 'nim-rx-constituents
               (cons 'int-lit
                     (nim-rx (or hex-lit dec-lit oct-lit bin-lit))))
  (add-to-list 'nim-rx-constituents
               (cons 'float-lit
                     (nim-rx
                      digit (0+ (or "_" digit))
                      (? (and "." (1+ (or "_" digit))))
                      (? exponent))))

  (add-to-list 'nim-rx-constituents
               (cons 'float-suffix
                     (nim-rx
                      (group (or (and (in "fF") (or "32" "64" "128")) (in "dD"))))))

  (add-to-list 'nim-rx-constituents
               (cons 'nim-numbers
                     (nim-rx
                      symbol-start
                      (or
                       ;; float hex
                       (group (group hex-lit)
                              ;; "'" isn’t optional
                              (group "'" float-suffix))
                       ;; float
                       (group (group (or float-lit dec-lit oct-lit bin-lit))
                              (group (? "'") float-suffix))
                       ;; u?int
                       (group
                        (group int-lit)
                        (? (group (? "'")
                                  (or (and (in "uUiI") (or "8" "16" "32" "64"))
                                      (in "uU"))))))
                      symbol-end)))

  ;; pragma
  (add-to-list
   'nim-rx-constituents
   (cons 'pragma
         (nim-rx
          (group "{."
                 (minimal-match
                  (1+ (or
                       ;; any string
                       ;; (emit pragma can include ".}")
                       string
                       any "\n")))
                 (? ".") "}"))))

  (add-to-list 'nim-rx-constituents
               (cons 'block-start (nim-rx (or decl-block block-start-defun))))

  (add-to-list 'nim-rx-constituents
               (cons 'font-lock-defun
                     (nim-rx defun
                             (? (group (1+ " ") (or identifier quoted-chars)
                                       (0+ " ") (? (group "*"))))
                             (? (minimal-match
                                 (group (0+ " ") "[" (0+ (or any "\n")) "]")))
                             (? (minimal-match
                                 (group (0+ " ") "(" (0+ (or any "\n")) ")")))
                             ;; return type
                             (? (group (0+ " ") ":" (0+ " ")
                                       (? (group "var " (0+ " ")))
                                       (? (group (or "ref" "ptr") " " (* " ")))
                                       (group identifier))))))

  (add-to-list 'nim-rx-constituents
               (cons 'colon-type
                     (nim-rx (or identifier quoted-chars) (? "*")
                             (* " ") ":" (* " ")
                             (? (and "var " (0+ " ")))
                             (? (group (and (or "ref" "ptr") " " (* " "))))
                             (group identifier))))

  (add-to-list 'nim-rx-constituents
               (cons 'backquoted-chars
                     (rx
                      (syntax expression-prefix)
                      (minimal-match (1+ (not (syntax comment-end))))
                      (syntax expression-prefix))))

  (add-to-list 'nim-rx-constituents
               (cons 'backticks
                     (nim-rx
                      (or line-start " " (syntax open-parenthesis))
                      (group (or (group (syntax expression-prefix)
                                        backquoted-chars
                                        (syntax expression-prefix))
                                 (group backquoted-chars)))
                      (or " "
                          line-end
                          (syntax punctuation)
                          (syntax comment-end)
                          (syntax symbol)
                          (syntax open-parenthesis)
                          (syntax close-parenthesis)))))

  (add-to-list 'nim-rx-constituents
               (cons 'font-lock-export
                     (nim-rx line-start (1+ " ")
                             (group
                              (or identifier quoted-chars) "*"
                              (? (and "[" word "]"))
                              (0+ (and "," (? (0+ " "))
                                       (or identifier quoted-chars) "*")))
                             (0+ " ") (or ":" "{." "=") (0+ nonl)
                             line-end)))

  ) ; end of eval-and-compile

(provide 'nim-rx)
;;; nim-rx.el ends here
