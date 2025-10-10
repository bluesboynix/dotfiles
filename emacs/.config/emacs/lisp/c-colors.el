;;; c-colors.el --- Custom C/C++ syntax highlighting -*- lexical-binding: t; -*-

(defface c-keyword-face
  '((t (:foreground "#c678dd" :weight bold))) ; purple
  "Face for C/C++ keywords.")

(defface c-type-face
  '((t (:foreground "#56b6c2" :weight bold))) ; cyan
  "Face for C/C++ types.")

(defface c-function-face
  '((t (:foreground "#61afef"))) ; blue
  "Face for C/C++ function names.")

(defface c-constant-face
  '((t (:foreground "#d19a66"))) ; gold
  "Face for numeric and symbolic constants.")

(defface c-string-face
  '((t (:foreground "#e5c07b"))) ; yellow
  "Face for string literals.")

(defface c-comment-face
  '((t (:foreground "#5c6370" :slant italic))) ; grey
  "Face for comments.")

(defface c-preprocessor-face
  '((t (:foreground "#98c379" :weight bold))) ; green
  "Face for preprocessor directives (#include, #define, etc).")

(defface c-macro-face
  '((t (:foreground "#e06c75" :weight bold))) ; red
  "Face for macros and defines.")

(defface c-number-face
  '((t (:foreground "#d19a66"))) ; orange-gold
  "Face for numeric literals.")

(defface c-operator-face
  '((t (:foreground "#abb2bf"))) ; light gray
  "Face for operators.")

(defface c-variable-face
  '((t (:foreground "#c0c0c0"))) ; silver
  "Face for local variables (heuristic).")

;; ---------------------------------------------------------------------------

(defconst c-color-font-lock-keywords
  (let* (
         ;; keywords
         (keywords
          '("if" "else" "while" "for" "switch" "case" "break" "continue"
            "return" "goto" "sizeof" "typedef" "static" "inline" "volatile"
            "const" "struct" "union" "enum" "extern" "register" "restrict"
            "default" "do"))

         ;; types
         (types
          '("int" "char" "float" "double" "short" "long" "signed" "unsigned"
            "void" "bool" "size_t" "ptrdiff_t" "auto" "class" "template"
            "typename" "namespace" "public" "private" "protected" "virtual"
            "friend" "using" "new" "delete" "this" "operator" "mutable"))

         ;; constants
         (constants
          '("NULL" "true" "false" "nullptr" "stdin" "stdout" "stderr" "EOF"))

         ;; preprocessor
         (preproc
          '("define" "include" "ifdef" "ifndef" "endif" "undef" "pragma" "error" "line")))

    `(
      ;; keywords
      (,(regexp-opt keywords 'symbols) . 'c-keyword-face)
      ;; types
      (,(regexp-opt types 'symbols) . 'c-type-face)
      ;; constants
      (,(regexp-opt constants 'symbols) . 'c-constant-face)
      ;; preprocessor directives
      ("^\\s-*#\\s-*\\(\\w+\\)" 1 'c-preprocessor-face)
      ;; macro definitions
      ("\\<[A-Z_][A-Z0-9_]+\\>" . 'c-macro-face)
      ;; function names (simple heuristic)
      ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 'c-function-face)
      ;; numbers
      ("\\b[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?[fFlLuU]*\\b" . 'c-number-face)
      ;; strings
      ("\".*?\"" . 'c-string-face)
      ;; chars
      ("'.'" . 'c-string-face)
      ;; comments
      ("//.*$" . 'c-comment-face)
      ("/\\*\\([^*]\\|\\*[^/]\\)*\\*/" . 'c-comment-face)
      ;; operators
      ("[-+*/%=<>!&|^~?:]" . 'c-operator-face)
      )))

;; ---------------------------------------------------------------------------

(defun c-color-apply ()
  "Enable custom C/C++ highlighting."
  (font-lock-add-keywords nil c-color-font-lock-keywords))

(add-hook 'c-mode-hook #'c-color-apply)
(add-hook 'c++-mode-hook #'c-color-apply)

(provide 'c-colors)
;;; c-colors.el ends here
