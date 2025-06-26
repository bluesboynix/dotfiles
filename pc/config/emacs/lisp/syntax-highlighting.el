;;; syntax-highlighting.el --- Doom One style Common Lisp syntax highlighting -*- lexical-binding: t -*-

;;; Commentary:

;; This Emacs Lisp script provides enhanced syntax highlighting
;; for Common Lisp code using the Doom One color palette.
;;
;; Features include:
;; - Distinct colors for booleans, numbers, keywords, characters, and strings.
;; - Highlighting the first symbol of lists (function and macro names).
;; - Special coloring for quoted symbols and quoted lists.
;; - Highlighting common type predicates (e.g., stringp, listp).
;; - Additional highlighting for macros, special forms, and user-defined functions.
;;
;; The highlighting is applied to `lisp-mode` and `lisp-interaction-mode`,
;; making it suitable for editing Common Lisp source files and interactive Lisp buffers.
;;
;; To use, load this file in your Emacs configuration and the highlighting
;; rules will automatically activate for Lisp buffers.

;;; Code:

(setq font-lock-maximum-decoration t)

;;; Faces based on Doom One palette

(defface font-lock-comment-face
  '((t (:foreground "#5c6370" :slant italic)))
  "Face for comments with dark grey color and italic style."
  :group 'font-lock-faces)

(defface cl-boolean-face
  '((t (:foreground "#98be65" :weight bold))) ; green
  "Face for booleans: t, nil.")

(defface cl-number-face
  '((t (:foreground "#da8548"))) ; orange
  "Face for numeric literals.")

(defface cl-keyword-face
  '((t (:foreground "#c678dd" :slant italic))) ; purple
  "Face for keywords like :key.")

(defface cl-character-face
  '((t (:foreground "#ff6c6b"))) ; red
  "Face for character literals like #\\A.")

(defface cl-string-face
  '((t (:foreground "#98be65"))) ; yellow
  "Face for strings.")

(defface cl-list-head-face
  '((t (:foreground "#51afef" :weight bold))) ; bright blue
  "Face for the first symbol of lists (functions/macros).")

(defface cl-quoted-face
  '((t (:foreground "#98be65" :slant italic))) ; green italic
  "Face for quoted symbols and lists.")

(defface cl-type-predicate-face
  '((t (:foreground "#51afef" :weight bold))) ; bright blue (same as list head)
  "Face for type predicates (stringp, listp, etc.).")

(defface cl-macro-face
  '((t (:foreground "#d19a66" :weight bold :underline t))) ; orange, bold, underlined
  "Face for macros.")

(defface cl-special-form-face
  '((t (:foreground "#56b6c2" :weight bold))) ; cyan, bold
  "Face for special forms.")

(defface cl-custom-fn-face
  '((t (:foreground "#98be65" :weight bold :italic t))) ; green, bold italic
  "Face for user-defined functions.")

;;; Regexes for atoms

(defconst cl-boolean-regexp
  "\\_<\\(t\\|nil\\)\\_>")

(defconst cl-number-regexp
  "\\_<[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\_>")

(defconst cl-keyword-regexp
  "\\_<:\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")

(defconst cl-character-regexp
  "#\\\\\\(?:.\\|newline\\|space\\|tab\\|backspace\\|return\\|rubout\\)")

(defconst cl-string-regexp
  "\"\\(?:\\\\.\\|[^\"\\]\\)*\"")

;;; List head regexp (first symbol after '(' possibly with spaces)

(defconst cl-list-head-regexp
  "(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)")

;;; Quoted symbols and lists: 'foo or '(a b)

(defconst cl-quoted-regexp
  "'\\(\\(?:\\sw\\|\\s_\\)+\\)")

;;; Type predicates list - add more if you want

(defconst cl-type-predicates
  '("stringp" "listp" "numberp" "symbolp" "functionp" "arrayp" "vectorp" "hash-table-p"))

(defconst cl-type-predicate-regexp
  (concat "\\_<" (regexp-opt cl-type-predicates t) "\\_>"))

;;; Macros, special forms, and custom functions

(defconst cl-macros
  '("defmacro" "macrolet" "symbol-macrolet" "and" "or" "when" "unless"))

(defconst cl-special-forms
  '("if" "let" "let*" "setq" "quote" "progn" "block" "return" "catch" "throw"))

(defconst cl-custom-functions
  '("my-fn" "custom-add" "compute-thing")) ; add your custom function names here

(defconst cl-macros-regexp
  (concat "(" (regexp-opt cl-macros t) "\\_>"))

(defconst cl-special-forms-regexp
  (concat "(" (regexp-opt cl-special-forms t) "\\_>"))

(defconst cl-custom-fns-regexp
  (concat "(" (regexp-opt cl-custom-functions t) "\\_>"))

;;; Add font-lock keywords to Lisp modes

(dolist (mode '(lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords mode
    `(
      ;; Macros (first symbol in list)
      (,cl-macros-regexp . 'cl-macro-face)
      ;; Special forms (first symbol in list)
      (,cl-special-forms-regexp . 'cl-special-form-face)
      ;; Custom functions (first symbol in list)
      (,cl-custom-fns-regexp . 'cl-custom-fn-face)
      ;; Booleans
      (,cl-boolean-regexp      . 'cl-boolean-face)
      ;; Numbers
      (,cl-number-regexp       . 'cl-number-face)
      ;; Keywords
      (,cl-keyword-regexp      . 'cl-keyword-face)
      ;; Characters
      (,cl-character-regexp    . 'cl-character-face)
      ;; Strings
      (,cl-string-regexp       . 'cl-string-face)
      ;; List heads (generic)
      (,cl-list-head-regexp 1 'cl-list-head-face)
      ;; Quoted symbols
      (,cl-quoted-regexp 1 'cl-quoted-face)
      ;; Type predicates
      (,cl-type-predicate-regexp . 'cl-type-predicate-face)
      )))

;; Ensure comment face uses dark grey after theme loads
(with-eval-after-load 'doom-themes
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#5c5c5c"
                      :slant 'italic))


(provide 'syntax-highlighting)
;;; syntax-highlighting.el ends here
