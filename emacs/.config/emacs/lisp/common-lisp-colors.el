;;; syntax-highlighting.el --- Doom One style Common Lisp syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced Common Lisp syntax highlighting using the Doom One color palette.

;;; Code:

(setq font-lock-maximum-decoration t)

;; ---------------------------------------------------------------------------
;; Face Definitions
;; ---------------------------------------------------------------------------

(defface cl-special-form-face
  '((t (:foreground "#00aa00")))
  "Face for Common Lisp special forms.")

(defface cl-macro-face
  '((t (:foreground "#7b30ff")))
  "Face for standard macros.")

(defface cl-function-face
  '((t (:foreground "#66ccff")))
  "Face for standard built-in functions.")

(defface cl-user-function-face
  '((t (:foreground "#ff00ff")))
  "Face for user-defined functions (names in defun).")

(defface cl-constant-face
  '((t (:foreground "#ff5555")))
  "Face for symbols declared with defconstant/defparameter.")

(defface cl-quoted-face
  '((t (:foreground "#90ee90" :slant italic)))
  "Face for quoted symbols and lists.")

(defface cl-boolean-face
  '((t (:foreground "#ffff00")))
  "Face for booleans: T and NIL.")

(defface cl-operator-face
  '((t (:foreground "#ffa500")))
  "Face for arithmetic/comparison operators (+, -, =, etc.).")

(defface cl-first-form-face
  '((t (:foreground "#00f0a0")))
  "Face for first element in forms not caught by other rules.")

(defface cl-number-face
  '((t (:foreground "#b0c4de" :slant italic)))
  "Face for numbers and hex literals.")

;; ---------------------------------------------------------------------------
;; Regex Definitions
;; ---------------------------------------------------------------------------

(defconst cl-user-defun-regexp
  "(\\s-*defun\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match a user-defined function name inside defun.")

(defconst cl-constant-regexp
  "(\\s-*\\(?:defconstant\\|defparameter\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match constant names in defconstant/defparameter.")

(defconst cl-number-regexp
  "\\_<\\(?:[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\|#x[0-9a-fA-F]+\\)\\_>"
  "Match integers, floats, or hex numbers.")

(defconst cl-generic-form-regexp
  "(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match the first symbol of a generic form.")

(defconst cl-quoted-regexp
  "'\\(\\(?:\\sw\\|\\s_\\)+\\)\\|'(\\(?:.\\|\n\\)*?)"
  "Match quoted symbols or quoted lists.")

(defconst cl-boolean-regexp
  "\\_<\\(?:t\\|nil\\)\\_>"
  "Match booleans T and NIL.")

(defconst cl-operator-regexp
  "\\_<\\(?:\\+\\|-\\|\\*\\|/\\|=\\|/=\\|<\\|<=\\|>\\|>=\\)\\_>"
  "Match arithmetic and comparison operators.")

;; ---------------------------------------------------------------------------
;; Keyword Lists
;; ---------------------------------------------------------------------------

(defconst cl-special-forms
  '("block" "catch" "declare" "eval-when" "flet" "function"
    "go" "if" "labels" "let" "let*" "load-time-value" "locally"
    "macrolet" "progn" "progv" "quote" "return-from" "setq"
    "symbol-macrolet" "tagbody" "the" "throw" "unwind-protect")
  "Common Lisp special forms.")

(defconst cl-standard-macros
  '("defun" "defmacro"
    "destructuring-bind" "multiple-value-bind" "multiple-value-setq"
    "with-open-file" "with-open-stream" "with-input-from-string"
    "with-output-to-string" "with-slots" "with-accessors"
    "with-hash-table-iterator" "with-standard-io-syntax"
    "when" "unless" "and" "or" "cond" "case" "ecase" "typecase"
    "etypecase" "ctypecase" "loop" "do" "do*" "dotimes" "dolist"
    "assert" "check-type" "declaim" "define-compiler-macro"
    "define-condition" "define-modify-macro" "define-setf-expander"
    "define-symbol-macro" "define-method-combination" "defclass"
    "defgeneric" "defmethod" "defpackage" "defparameter"
    "defvar" "defconstant" "time" "ignore-errors" "handler-case"
    "handler-bind" "restart-case" "restart-bind" "prog1" "prog2"
    "shiftf" "rotatef" "psetf" "psetq" "setf" "remf")
  "Standard macros.")

(defconst cl-standard-functions
  '("car" "cdr" "cons" "append" "list" "member" "assoc" "mapcar"
    "reduce" "remove" "length" "reverse" "apply" "funcall" "eq"
    "eql" "equal" "type-of" "symbol-name" "make-array" "aref"
    "format" "read" "print" "write" "load" "find" "position"
    "copy-list" "sort")
  "Standard built-in functions.")

(defconst cl-special-form-regexp
  (concat "(" (regexp-opt cl-special-forms t) "\\_>")
  "Regexp for special forms.")

(defconst cl-macro-regexp
  (concat "(" (regexp-opt cl-standard-macros t) "\\_>")
  "Regexp for standard macros.")

(defconst cl-function-regexp
  (concat "(" (regexp-opt cl-standard-functions t) "\\_>")
  "Regexp for standard built-in functions.")

;; ---------------------------------------------------------------------------
;; Font-Lock Rules
;; ---------------------------------------------------------------------------

(dolist (mode '(lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   `(
     (,cl-special-form-regexp       . 'cl-special-form-face)
     (,cl-macro-regexp              . 'cl-macro-face)
     (,cl-function-regexp           . 'cl-function-face)
     (,cl-user-defun-regexp 1       'cl-user-function-face)
     (,cl-constant-regexp   1       'cl-constant-face)
     (,cl-quoted-regexp             . 'cl-quoted-face)
     (,cl-boolean-regexp            . 'cl-boolean-face)
     (,cl-operator-regexp           . 'cl-operator-face)
     (,cl-generic-form-regexp 1     'cl-first-form-face)
     (,cl-number-regexp             . 'cl-number-face))))

;; ---------------------------------------------------------------------------
;; Comment Face Tweaking (after Doom themes load)
;; ---------------------------------------------------------------------------

(with-eval-after-load 'doom-themes
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#5c5c5c"
                      :slant 'italic))

(provide 'common-lisp-colors)

;;; syntax-highlighting.el ends here
