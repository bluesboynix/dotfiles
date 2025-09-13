;;; scheme-colors.el --- Enhanced Scheme syntax highlighting -*- lexical-binding: t; -*-

(setq font-lock-maximum-decoration t)

;; --------------------------
;; Custom Faces
;; --------------------------
(defface scheme-keyword-face
  '((t :foreground "#d010d0"))
  "Face for Scheme keywords (define, lambda, etc.).")

(defface scheme-function-face
  '((t :foreground "#00ffaf"))
  "Face for Scheme function names immediately after define.")

(defface scheme-builtin-face
  '((t :foreground "#10a0f0"))
  "Face for built-in Scheme procedures like display, newline, car, cdr, etc.")

(defface scheme-constant-face
  '((t :foreground "#aadd00"))
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

(defface scheme-quoted-content
  '((t :slant italic :foreground "#98be65"))
  "Face for everything inside quoted forms.")

(defface scheme-first-symbol-face
  '((t :foreground "#50ff50"))
  "Face for the first symbol in any s-expression (except those already highlighted).")


;; --------------------------
;; Font-lock Keywords
;; --------------------------
(defvar scheme-font-lock-keywords
  `(
    ;; Handles 'atom, '( ... ), (quote ( ... ))
    ("('\\(?:\\(?:\\sw\\|\\s_\\)+\\|([^)]*)\\))" . 'scheme-quoted-content)
    ("(quote[ \t]+\\([^)]*\\))" 1 'scheme-quoted-content)

    ;; Highlight the quote character itself
    ("\\('\\)" 1 'scheme-quote-face)
    
    ;; Special forms / Keywords
    (,(regexp-opt
       '("define" "define-values" "define-syntax"
         "lambda" "if" "cond" "else" "case"
         "and" "or" "let" "let*" "let-values" "let*-values" "letrec"
         "do" "delay" "begin" "set!" "quote"
         "syntax-rules" "parameterize" "guard") 'symbols)
     . 'scheme-keyword-face)

    ;; Built-in functions
    (,(regexp-opt
       '("car" "cdr" "cons" "list" "length" "append"
	 "display" "newline" "eval" "apply" "map" "foldl" "foldr"
	 "call/cc" "force" "read" "read-line" "eq?" "eqv?" "equal?" "not"
	 ;; Type predicates
	 "boolean?" "symbol?" "char?" "string?" "vector?"
	 "pair?" "list?" "null?"
	 "number?" "complex?" "real?" "rational?" "integer?"
	 "exact?" "inexact?" "procedure?" "port?" "input-port?"
	 "output-port?" "bytevector?"
	 ;; Number predicates
	 "zero?" "positive?" "negative?" "odd?" "even?"
	 "finite?" "infinite?" "nan?"
	 ;; Miscellaneous
	 "eof-object?"
	 "char-alphabetic?" "char-numeric?" "char-whitespace?"
	 "char-upper-case?" "char-lower-case?"
	 ;; Common list utilities often used as predicates
	 "memq" "memv" "member" "assq" "assv" "assoc")
       'symbols)
     . 'scheme-builtin-face)

    
    ;; Function names immediately after define / define-values
    ("(define\\(?:-values\\)?[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 'scheme-function-face))

    ;; Booleans
    ("\\(#t\\|#f\\)" . 'scheme-constant-face)

    ;; Numbers (integer and float, with optional sign)
    ("\\_<[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:/[0-9]+\\)?\\_>" . 'scheme-constant-face)

    ;; Nil / empty list
    ("'()" . 'scheme-constant-face)

    ;; Strings
    ("\".*?\"" . 'scheme-string-face)

    ;; Comments
    (";.*" . 'scheme-comment-face)

    ;; First symbol in any S-expression (fallback)
    ("(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 (unless (or
                 ;; do NOT recolor if already highlighted by higher-priority rules
                 (get-text-property (match-beginning 1) 'face))
          'scheme-first-symbol-face)))
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
