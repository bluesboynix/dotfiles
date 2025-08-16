;;; scheme-colors.el --- Enhanced Scheme syntax highlighting -*- lexical-binding: t; -*-

(setq font-lock-maximum-decoration t)

;; --------------------------
;; Custom Faces
;; --------------------------
(defface scheme-keyword-face
  '((t :foreground "#a020f0" :weight bold))
  "Face for Scheme keywords (define, lambda, etc.).")

(defface scheme-function-face
  '((t :foreground "#51afef"))
  "Face for Scheme function names immediately after define.")

(defface scheme-variable-face
  '((t :foreground "#c678dd"))
  "Face for Scheme variables (lambda or let bindings).")

(defface scheme-builtin-face
  '((t :foreground "#46d9ff"))
  "Face for built-in Scheme procedures like display, newline, car, cdr, etc.")

(defface scheme-constant-face
  '((t :foreground "#98be65"))
  "Face for constants like #t, #f, numbers, nil.")

(defface scheme-string-face
  '((t :foreground "#da8548"))
  "Face for strings.")

(defface scheme-comment-face
  '((t :foreground "#5B6268" :slant italic))
  "Face for comments.")

(defface scheme-quote-face
  '((t :foreground "#98be65" :slant italic))
  "Face for the quote symbol `'` in Scheme.")

;; --------------------------
;; Font-lock Keywords
;; --------------------------
(defvar scheme-font-lock-keywords
  `(
    ;; 1. Quote symbol first
    ("\\('\\)\\(?:\\sw\\|\\s_\\|\\s(\\)" 1 'scheme-quote-face)

    ;; 2. Special forms / Keywords
    (,(regexp-opt
       '("define" "define-values" "define-syntax"
         "lambda" "if" "cond" "else" "case"
         "and" "or" "let" "let*" "let-values" "let*-values" "letrec"
         "do" "delay" "begin" "set!" "quote"
         "syntax-rules" "parameterize" "guard") 'symbols)
     . 'scheme-keyword-face)

    ;; 3. Built-in functions
    (,(regexp-opt
       '("car" "cdr" "cons" "list" "length" "append"
         "display" "newline" "eval" "apply" "map" "foldl" "foldr"
         "call/cc" "force" "read" "read-line" "eq?" "equal?" "not"
         "number?" "symbol?" "boolean?" "procedure?" "string?"
         "vector?" "list?" "pair?") 'symbols)
     . 'scheme-builtin-face)

    ;; 4. Function names immediately after define / define-values
    ("(define\\(?:-values\\)?[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 'scheme-function-face))

    ;; 5. Variable names in lambda or let bindings
    ("(\\(?:lambda\\|let\\*?\\|let-values\\|let*-values\\)[ \t]*(\\([^)]*\\))"
     (1 'scheme-variable-face))

    ;; 6. Booleans
    ("\\(#t\\|#f\\)" . 'scheme-constant-face)

    ;; 7. Numbers (integer and float)
    ("\\_<[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . 'scheme-constant-face)

    ;; 8. Nil / empty list
    ("'()" . 'scheme-constant-face)

    ;; 9. Strings
    ("\".*?\"" . 'scheme-string-face)

    ;; 10. Comments
    (";.*" . 'scheme-comment-face)
    ))

;; --------------------------
;; Setup Function
;; --------------------------
(defun scheme-colors-setup ()
  "Apply enhanced syntax highlighting for Scheme."
  (font-lock-add-keywords nil scheme-font-lock-keywords))

(add-hook 'scheme-mode-hook #'scheme-colors-setup)
(add-hook 'scheme-ts-mode-hook #'scheme-colors-setup)

;; --------------------------
;; Rainbow Delimiters
;; --------------------------
(defun my-scheme-rainbow-setup ()
  "Enable rainbow-delimiters with custom colors for Scheme."
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode 1)
  ;; Custom colors
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FF0000")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FF8C00")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#FFFF00")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#00FF00")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#56B6C2")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#9467BD")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#D19A66")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "white" :background "#FF0000" :weight 'bold))

(add-hook 'scheme-mode-hook #'my-scheme-rainbow-setup)
(add-hook 'scheme-ts-mode-hook #'my-scheme-rainbow-setup)

(provide 'scheme-colors)
;;; scheme-colors.el ends here
