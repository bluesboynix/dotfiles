;;; syntax-highlighting.el --- Doom One style Common Lisp syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; This Emacs Lisp script provides enhanced syntax highlighting
;; for Common Lisp code using the Doom One color palette.

;;; Code:

(setq font-lock-maximum-decoration t)

;;;; Face Definitions

(defface cl-special-form-face
  '((t (:foreground "#a678de" :weight bold))) ; violet
  "Face for Common Lisp special forms.")

(defface cl-macro-face
  '((t (:foreground "#6d5acf" :weight bold))) ; indigo
  "Face for standard macros.")

(defface cl-function-face
  '((t (:foreground "#0d80ba" :weight bold))) ; blue
  "Face for standard built-in functions.")

(defface cl-user-function-face
  '((t (:foreground "#66ccff" :slant italic :weight semi-bold))) ; light blue
  "Face for user-defined Common Lisp functions.")

(defface cl-quoted-face
  '((t (:foreground "#39ff14" :slant italic))) ; green
  "Face for quoted symbols and lists.")

(defface cl-boolean-face
  '((t (:foreground "#ffff00" :weight bold))) ; yellow
  "Face for booleans: T and NIL.")

(defface cl-operator-face
  '((t (:foreground "#ffa500" :weight bold))) ; orange
  "Face for symbol operators like +, -, =, *, etc.")

(defface cl-first-form-face
  '((t (:foreground "#afa9f0" :weight semi-bold)))
  "Face for first element in forms not caught by other rules.")

(defface cl-number-face
  '((t (:slant italic :foreground "#b0c4de"))) ; soft blue
  "Face for numbers and hex literals.")

;;;; Regex Definitions

(defconst cl-user-defun-regexp
  "(\\s-*defun\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)")

(defconst cl-number-regexp
  "\\_<\\(?:[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\|#x[0-9a-fA-F]+\\)\\_>")

(defconst cl-generic-form-regexp
  "(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)")

(defconst cl-quoted-regexp
  "'\\(\\(?:\\sw\\|\\s_\\)+\\)\\|'(\\(?:.\\|\n\\)*?)")

(defconst cl-boolean-regexp
  "\\_<\\(?:t\\|nil\\)\\_>")

(defconst cl-operator-regexp
  "\\_<\\(?:\\+\\|-\\|\\*\\|/\\|=\\|/=\\|<\\|<=\\|>\\|>=\\)\\_>")

;;;; Lists and Their Regex Variants

(defconst cl-special-forms
  '("block" "catch" "declare" "eval-when" "flet" "function"
    "go" "if" "labels" "let" "let*" "load-time-value" "locally"
    "macrolet" "progn" "progv" "quote" "return-from" "setq"
    "symbol-macrolet" "tagbody" "the" "throw" "unwind-protect"))

(defconst cl-special-form-regexp
  (concat "(" (regexp-opt cl-special-forms t) "\\_>"))

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
    "shiftf" "rotatef" "psetf" "psetq" "setf" "remf"))

(defconst cl-macro-regexp
  (concat "(" (regexp-opt cl-standard-macros t) "\\_>"))

(defconst cl-standard-functions
  '("car" "cdr" "cons" "append" "list" "member" "assoc" "mapcar"
    "reduce" "remove" "length" "reverse" "apply" "funcall" "eq"
    "eql" "equal" "type-of" "symbol-name" "make-array" "aref"
    "format" "read" "print" "write" "load" "find" "position"
    "copy-list" "sort"))

(defconst cl-function-regexp
  (concat "(" (regexp-opt cl-standard-functions t) "\\_>"))

;;;; Font Lock Application

(dolist (mode '(lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   `(
     ;; Special Forms
     (,cl-special-form-regexp       . 'cl-special-form-face)

     ;; Standard Macros
     (,cl-macro-regexp              . 'cl-macro-face)

     ;; Standard Functions
     (,cl-function-regexp           . 'cl-function-face)

     ;; User-defined function name in defun
     (,cl-user-defun-regexp 1       'cl-user-function-face)

     ;; Quoted items
     (,cl-quoted-regexp             . 'cl-quoted-face)

     ;; Boolean
     (,cl-boolean-regexp            . 'cl-boolean-face)

     ;; Operators
     (,cl-operator-regexp           . 'cl-operator-face)

     ;; Fallback form head
     (,cl-generic-form-regexp 1     'cl-first-form-face)

     ;; Numbers
     (,cl-number-regexp             . 'cl-number-face)
     )))

;;;; Comment Face Tweaking (after Doom themes load)

(with-eval-after-load 'doom-themes
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#5c5c5c"
                      :slant 'italic))

(provide 'syntax-highlighting)

;;; syntax-highlighting.el ends here
