;;; lang-sweet-scheme.el --- SRFI-110 Sweet Scheme mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Major mode for editing SRFI-110 Sweet Scheme files.
;;
;; Features:
;;
;; - .sscm file association
;; - Inherits from scheme-mode
;; - R7RS-small syntax highlighting
;; - R7RS-small standard procedure highlighting
;; - Common Guile procedure highlighting
;; - SRFI-110 / Sweet Scheme syntax highlighting
;; - RET preserves current indentation
;; - TAB inserts exactly 2 spaces
;; - No automatic indentation rules
;;
;;; Code:

(require 'scheme)

;;;; -------------------------------------------------------------------
;;;; R7RS-small syntax and keywords
;;;; -------------------------------------------------------------------

(defconst sweet-scheme-keywords
  '(
    ;; Definitions
    "define"
    "define-syntax"
    "define-values"

    ;; Procedures
    "lambda"

    ;; Conditionals
    "if"
    "cond"
    "case"
    "else"
    "=>"

    ;; Bindings
    "let"
    "let*"
    "letrec"
    "letrec*"
    "let-values"
    "let*-values"

    ;; Sequencing and assignment
    "begin"
    "set!"

    ;; Boolean syntax
    "and"
    "or"

    ;; Iteration
    "do"

    ;; Quoting
    "quote"
    "quasiquote"
    "unquote"
    "unquote-splicing"

    ;; Macros
    "syntax-rules"

    ;; Libraries
    "import"
    "export"
    "include"
    "include-library-declarations"
    "define-library"

    ;; Exceptions
    "guard"

    ;; Dynamic parameters
    "parameterize"

    ;; Promises
    "delay"

    ;; Useful non-standard/common forms
    "when"
    "unless"

    ;; Boolean constants
    "#t"
    "#f")
  "R7RS-small syntax keywords.")


;;;; -------------------------------------------------------------------
;;;; R7RS-small standard procedures
;;;; -------------------------------------------------------------------

(defconst sweet-scheme-r7rs-procedures
  '(
    ;; ---------------------------------------------------------------
    ;; Equivalence predicates
    ;; ---------------------------------------------------------------
    "eq?"
    "eqv?"
    "equal?"

    ;; ---------------------------------------------------------------
    ;; Booleans
    ;; ---------------------------------------------------------------
    "not"
    "boolean?"

    ;; ---------------------------------------------------------------
    ;; Pairs and lists
    ;; ---------------------------------------------------------------
    "pair?"
    "cons"
    "car"
    "cdr"

    "caar"
    "cadr"
    "cdar"
    "cddr"

    "caaar"
    "caadr"
    "cadar"
    "caddr"
    "cdaar"
    "cdadr"
    "cddar"
    "cdddr"

    "caaaar"
    "caadar"
    "caaddr"
    "cadaar"
    "cadadr"
    "caddar"
    "cadddr"
    "cdaaar"
    "cdaadr"
    "cdadar"
    "cdaddr"
    "cddaar"
    "cddadr"
    "cdddar"
    "cddddr"

    "null?"
    "list?"
    "list"
    "make-list"
    "length"
    "append"
    "reverse"
    "list-tail"
    "list-ref"

    "memq"
    "memv"
    "member"

    "assq"
    "assv"
    "assoc"

    ;; ---------------------------------------------------------------
    ;; Higher-order procedures
    ;; ---------------------------------------------------------------
    "apply"
    "map"
    "for-each"
    "procedure?"

    ;; ---------------------------------------------------------------
    ;; Symbols
    ;; ---------------------------------------------------------------
    "symbol?"
    "symbol->string"
    "string->symbol"

    ;; ---------------------------------------------------------------
    ;; Numbers
    ;; ---------------------------------------------------------------
    "number?"
    "complex?"
    "real?"
    "rational?"
    "integer?"

    "exact?"
    "inexact?"

    "exact"
    "inexact"

    "zero?"
    "positive?"
    "negative?"

    "odd?"
    "even?"

    "max"
    "min"
    "abs"

    "floor"
    "ceiling"
    "truncate"
    "round"

    "quotient"
    "remainder"
    "modulo"

    "gcd"
    "lcm"

    "numerator"
    "denominator"

    "rationalize"

    "expt"
    "sqrt"

    "exp"
    "log"

    "sin"
    "cos"
    "tan"

    "asin"
    "acos"
    "atan"

    "make-rectangular"
    "make-polar"

    "real-part"
    "imag-part"
    "magnitude"
    "angle"

    ;; ---------------------------------------------------------------
    ;; Characters
    ;; ---------------------------------------------------------------
    "char?"

    "char=?"
    "char<?"
    "char>?"
    "char<=?"
    "char>=?"

    "char-ci=?"
    "char-ci<?"
    "char-ci>?"
    "char-ci<=?"
    "char-ci>=?"

    "char-alphabetic?"
    "char-numeric?"
    "char-whitespace?"
    "char-upper-case?"
    "char-lower-case?"

    "char-upcase"
    "char-downcase"
    "char-foldcase"

    "char->integer"
    "integer->char"

    ;; ---------------------------------------------------------------
    ;; Strings
    ;; ---------------------------------------------------------------
    "string?"
    "make-string"
    "string"

    "string-length"
    "string-ref"
    "string-set!"

    "string=?"
    "string<?"
    "string>?"
    "string<=?"
    "string>=?"

    "string-ci=?"
    "string-ci<?"
    "string-ci>?"
    "string-ci<=?"
    "string-ci>=?"

    "substring"
    "string-append"
    "string-copy"

    "string->list"
    "list->string"

    "string-for-each"

    "string-upcase"
    "string-downcase"
    "string-foldcase"

    ;; ---------------------------------------------------------------
    ;; Vectors
    ;; ---------------------------------------------------------------
    "vector?"
    "make-vector"
    "vector"

    "vector-length"
    "vector-ref"
    "vector-set!"

    "vector->list"
    "list->vector"

    "vector-map"
    "vector-for-each"

    ;; ---------------------------------------------------------------
    ;; Bytevectors
    ;; ---------------------------------------------------------------
    "bytevector?"
    "make-bytevector"
    "bytevector"

    "bytevector-length"
    "bytevector-u8-ref"
    "bytevector-u8-set!"

    "bytevector-copy"
    "bytevector-copy-partial"
    "bytevector-append"

    "utf8->string"
    "string->utf8"

    ;; ---------------------------------------------------------------
    ;; Multiple values and continuations
    ;; ---------------------------------------------------------------
    "values"
    "call-with-values"

    "call/cc"
    "call-with-current-continuation"

    "dynamic-wind"

    ;; ---------------------------------------------------------------
    ;; Promises
    ;; ---------------------------------------------------------------
    "force"
    "make-promise"

    ;; ---------------------------------------------------------------
    ;; Errors and exceptions
    ;; ---------------------------------------------------------------
    "error"

    "error-object?"
    "error-object-message"
    "error-object-irritants"

    "raise"
    "raise-continuable"
    "with-exception-handler"

    ;; ---------------------------------------------------------------
    ;; Ports
    ;; ---------------------------------------------------------------
    "port?"
    "input-port?"
    "output-port?"

    "textual-port?"
    "binary-port?"

    "port-open?"

    "close-port"
    "close-input-port"
    "close-output-port"

    ;; ---------------------------------------------------------------
    ;; Input
    ;; ---------------------------------------------------------------
    "read"
    "read-char"
    "peek-char"
    "char-ready?"

    "read-line"
    "read-string"

    "read-u8"
    "peek-u8"

    ;; ---------------------------------------------------------------
    ;; Output
    ;; ---------------------------------------------------------------
    "write"
    "write-shared"
    "write-simple"

    "display"
    "newline"

    "write-char"
    "write-string"
    "write-u8"

    ;; ---------------------------------------------------------------
    ;; File and port operations
    ;; ---------------------------------------------------------------
    "call-with-port"

    "call-with-input-file"
    "call-with-output-file"

    "open-input-file"
    "open-output-file"

    "open-input-string"
    "open-output-string"
    "get-output-string"

    "open-input-bytevector"
    "open-output-bytevector"
    "get-output-bytevector"

    "current-input-port"
    "current-output-port"
    "current-error-port"

    ;; ---------------------------------------------------------------
    ;; Evaluation
    ;; ---------------------------------------------------------------
    "eval"

    ;; ---------------------------------------------------------------
    ;; System
    ;; ---------------------------------------------------------------
    "exit"
    "emergency-exit")
  "R7RS-small standard procedures.")


;;;; -------------------------------------------------------------------
;;;; Common Guile procedures
;;;; -------------------------------------------------------------------

(defconst sweet-scheme-guile-procedures
  '(
    ;; Formatting
    "format"
    "simple-format"

    ;; Lists
    "filter"
    "filter-map"
    "fold"
    "fold-right"
    "any"
    "every"
    "list-copy"

    ;; Strings
    "string-trim"
    "string-trim-right"
    "string-trim-both"

    ;; Files
    "file-exists?"
    "delete-file"

    ;; Environment
    "getenv"
    "setenv"

    ;; Hash tables
    "make-hash-table"
    "hash-ref"
    "hash-set!"
    "hash-remove!"
    "hash-clear!"

    ;; Useful procedures
    "identity"
    "compose")
  "Common Guile procedures.")


;;;; -------------------------------------------------------------------
;;;; Font Lock rules
;;;; -------------------------------------------------------------------

(defvar sweet-scheme-font-lock-keywords
  `(

    ;; ---------------------------------------------------------------
    ;; SRFI-110 reader directive
    ;;
    ;; #!sweet
    ;; ---------------------------------------------------------------

    ("^\\s-*\\(#!sweet\\)\\b"
     1 font-lock-preprocessor-face)

    ;; ---------------------------------------------------------------
    ;; Sweet Scheme function definition
    ;;
    ;; define factorial(n)
    ;; ---------------------------------------------------------------

    ("^\\s-*define\\s-+\\([[:word:]_!?-]+\\)\\s-*("
     1 font-lock-function-name-face)

    ;; ---------------------------------------------------------------
    ;; R7RS keywords
    ;; ---------------------------------------------------------------

    (,(regexp-opt sweet-scheme-keywords 'symbols)
     . font-lock-keyword-face)

    ;; ---------------------------------------------------------------
    ;; R7RS standard procedures
    ;; ---------------------------------------------------------------

    (,(regexp-opt sweet-scheme-r7rs-procedures 'symbols)
     . font-lock-builtin-face)

    ;; ---------------------------------------------------------------
    ;; Common Guile procedures
    ;; ---------------------------------------------------------------

    (,(regexp-opt sweet-scheme-guile-procedures 'symbols)
     . font-lock-builtin-face)

    ;; ---------------------------------------------------------------
    ;; User function calls
    ;;
    ;; factorial(5)
    ;; main()
    ;;
    ;; Put this AFTER built-ins. Built-in procedures may still use
    ;; their builtin face depending on Font Lock precedence.
    ;; ---------------------------------------------------------------

    ("\\_<\\([[:alpha:]_][[:alnum:]_!?-]*\\)\\s-*("
     1 font-lock-function-name-face)

    ;; ---------------------------------------------------------------
    ;; Symbolic operators
    ;;
    ;; +  -  *  /  <=  >=  =
    ;; ---------------------------------------------------------------

    ("[+*/=<>-]+"
     . font-lock-builtin-face)

    ;; ---------------------------------------------------------------
    ;; SRFI-105 / SRFI-110 curly infix delimiters
    ;;
    ;; { expression }
    ;; ---------------------------------------------------------------

    ("[{}]"
     . font-lock-bracket-face))

  "Font Lock rules for `sweet-scheme-mode`.")


;;;; -------------------------------------------------------------------
;;;; Manual editing commands
;;;; -------------------------------------------------------------------

(defun sweet-scheme-tab ()
  "Insert exactly two spaces."
  (interactive)
  (insert "  "))


(defun sweet-scheme-newline ()
  "Insert a newline preserving the current indentation.

No automatic Sweet Scheme indentation is attempted."
  (interactive)
  (let ((indent (current-indentation)))
    (delete-horizontal-space t)
    (newline)
    (insert (make-string indent ?\s))))


;;;; -------------------------------------------------------------------
;;;; Major mode
;;;; -------------------------------------------------------------------

(define-derived-mode sweet-scheme-mode scheme-mode "Sweet-Scheme"
  "Major mode for editing SRFI-110 Sweet Scheme files.

RET preserves the indentation of the current line.
TAB inserts two spaces.
No automatic indentation rules are used."

  ;; Always use spaces.
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)

  ;; Add Sweet Scheme highlighting while keeping Scheme highlighting.
  (font-lock-add-keywords
   nil
   sweet-scheme-font-lock-keywords
   'append)

  ;; Refresh Font Lock.
  (font-lock-flush))


;;;; -------------------------------------------------------------------
;;;; Keybindings
;;;; -------------------------------------------------------------------

(define-key sweet-scheme-mode-map
            (kbd "TAB")
            #'sweet-scheme-tab)

(define-key sweet-scheme-mode-map
            (kbd "<tab>")
            #'sweet-scheme-tab)

(define-key sweet-scheme-mode-map
            (kbd "RET")
            #'sweet-scheme-newline)


;;;; -------------------------------------------------------------------
;;;; File association
;;;; -------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.sscm\\'" . sweet-scheme-mode))


(provide 'lang-sweet-scheme)

;;; lang-sweet-scheme.el ends here
