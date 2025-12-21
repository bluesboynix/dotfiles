;;; lang-common-lisp-colors.el --- Harmonized Common Lisp syntax highlighting -*- lexical-binding: t; -*-
;;; Commentary:
;; Elegant, high-contrast Lisp highlighting for Genesis Dark.
;; Harmonized with Rust and C/C++ schemes.
;; Emphasizes readability, structure, and calm contrast.
;;; Code:

(setq font-lock-maximum-decoration t)

;; -------------------------------------------------------------------
;; Faces
;; -------------------------------------------------------------------

(defface cl-special-form-face
  '((t (:foreground "#ff9f40" :weight semi-bold))) ; orange-gold
  "Face for Common Lisp special forms (if, let, progn, etc.).")

(defface cl-macro-face
  '((t (:foreground "#ffaaff" :weight semi-bold))) ; pinkish violet
  "Face for Lisp macros (defmacro, loop, when, etc.).")

(defface cl-function-face
  '((t (:foreground "#8fbfff"))) ; light blue
  "Face for standard Lisp built-in functions (car, cons, format, etc.).")

(defface cl-user-function-face
  '((t (:foreground "#0fa0ff" :weight semi-bold))) ; cyan
  "Face for user-defined function names in defun.")

(defface cl-constant-face
  '((t (:foreground "#ffcb6b"))) ; soft yellow
  "Face for constants defined with defconstant/defparameter.")

(defface cl-quoted-face
  '((t (:foreground "#98c379" :slant italic))) ; green
  "Face for quoted symbols and lists.")

(defface cl-boolean-face
  '((t (:foreground "#d19a66"))) ; brownish-orange
  "Face for booleans (T, NIL).")

(defface cl-operator-face
  '((t (:foreground "#ffaa0f"))) ; amber
  "Face for operators (+, -, =, etc.).")

(defface cl-number-face
  '((t (:foreground "#e5c07b" :slant italic))) ; gold
  "Face for numeric literals.")

(defface cl-comment-face
  '((t (:foreground "#5c6370" :slant italic))) ; muted gray
  "Face for comments.")

;; -------------------------------------------------------------------
;; Regex Definitions
;; -------------------------------------------------------------------

(defconst cl-user-defun-regexp
  "(\\s-*defun\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match user-defined function names in defun.")

(defconst cl-constant-regexp
  "(\\s-*\\(?:defconstant\\|defparameter\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match constant names in defconstant/defparameter.")

(defconst cl-number-regexp
  "\\_<\\(?:[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\|#x[0-9a-fA-F]+\\)\\_>"
  "Match integers, floats, or hex numbers.")

(defconst cl-quoted-regexp
  "'\\(\\(?:\\sw\\|\\s_\\)+\\)\\|'(\\(?:.\\|\n\\)*?)"
  "Match quoted symbols or lists.")

(defconst cl-boolean-regexp
  "\\_<\\(?:t\\|nil\\)\\_>"
  "Match booleans T and NIL.")

(defconst cl-operator-regexp
  "\\_<\\(?:\\+\\|-\\|\\*\\|/\\|=\\|/=\\|<\\|<=\\|>\\|>=\\)\\_>"
  "Match arithmetic and comparison operators.")

(defconst cl-generic-form-regexp
  "(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)"
  "Match the first symbol of generic forms for fallback highlighting.")

;; -------------------------------------------------------------------
;; Keyword Lists
;; -------------------------------------------------------------------

(defconst cl-special-forms
  '("block" "catch" "declare" "eval-when" "flet" "function"
    "go" "if" "labels" "let" "let*" "load-time-value" "locally"
    "macrolet" "progn" "progv" "quote" "return-from" "setq"
    "symbol-macrolet" "tagbody" "the" "throw" "unwind-protect"))

(defconst cl-standard-macros
  '("defun" "defmacro" "destructuring-bind" "multiple-value-bind"
    "multiple-value-setq" "with-open-file" "with-open-stream"
    "with-input-from-string" "with-output-to-string" "with-slots"
    "with-accessors" "with-hash-table-iterator" "with-standard-io-syntax"
    "when" "unless" "and" "or" "cond" "case" "ecase" "typecase"
    "etypecase" "ctypecase" "loop" "do" "do*" "dotimes" "dolist"
    "assert" "check-type" "declaim" "define-condition" "defclass"
    "defgeneric" "defmethod" "defpackage" "defparameter" "defvar"
    "defconstant" "setf" "time" "ignore-errors" "handler-case"
    "handler-bind" "restart-case" "restart-bind"))

(defconst cl-standard-functions
  '("car" "cdr" "cons" "append" "list" "member" "assoc" "mapcar"
    "reduce" "remove" "length" "reverse" "apply" "funcall" "eq"
    "eql" "equal" "symbol-name" "make-array" "aref" "format"
    "read" "print" "write" "find" "position" "copy-list" "sort"))

;; -------------------------------------------------------------------
;; Font-Lock Integration
;; -------------------------------------------------------------------

(dolist (mode '(lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   `(
     (,(concat "(" (regexp-opt cl-special-forms t) "\\_>") . 'cl-special-form-face)
     (,(concat "(" (regexp-opt cl-standard-macros t) "\\_>") . 'cl-macro-face)
     (,(concat "(" (regexp-opt cl-standard-functions t) "\\_>") . 'cl-function-face)
     (,cl-user-defun-regexp 1 'cl-user-function-face)
     (,cl-constant-regexp   1 'cl-constant-face)
     (,cl-quoted-regexp       . 'cl-quoted-face)
     (,cl-boolean-regexp      . 'cl-boolean-face)
     (,cl-operator-regexp     . 'cl-operator-face)
     (,cl-number-regexp       . 'cl-number-face)
     (";.*$"                  . 'cl-comment-face)
     (,cl-generic-form-regexp 1 'cl-function-face))))

(provide 'lang-common-lisp-colors)
;;; common-lisp-colors.el ends here
