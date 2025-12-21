;;; lang-racket-colors.el --- Custom Racket syntax highlighting for Genesis Dark -*- lexical-binding: t; -*-

;; -----------------------------
;; Color palette (Genesis Dark)
;; -----------------------------
(defconst racket-colors-bg        "#000000")
(defconst racket-colors-fg        "#ffffff")
(defconst racket-colors-muted     "#b0b0b0")
(defconst racket-colors-comment   "#808080")
(defconst racket-colors-blue      "#0000ff")
(defconst racket-colors-lightblue "#afdfff")
(defconst racket-colors-green     "#00ff00")
(defconst racket-colors-midgreen  "#55cc55")
(defconst racket-colors-red       "#ff0000")
(defconst racket-colors-yellow    "#ffff00")
(defconst racket-colors-cyan      "#00ffff")
(defconst racket-colors-magenta   "#ffaaff")
(defconst racket-colors-violet    "#7f00ff")

;; -----------------------------
;; Define faces
;; -----------------------------
(defface racket-keyword-face
  `((t (:foreground ,racket-colors-violet)))
  "Face for Racket keywords (define, lambda, etc.).")

(defface racket-boolean-face
  `((t (:foreground ,racket-colors-green)))
  "Face for Racket booleans (#t, #f).")

(defface racket-number-face
  `((t (:foreground ,racket-colors-yellow)))
  "Face for numbers.")

(defface racket-string-face
  `((t (:foreground ,racket-colors-midgreen)))
  "Face for strings.")

(defface racket-builtin-face
  `((t (:foreground ,racket-colors-magenta)))
  "Face for built-in procedures.")

(defface racket-identifier-face
  `((t (:foreground ,racket-colors-lightblue)))
  "Face for general identifiers.")

(defface racket-comment-face
  `((t (:foreground ,racket-colors-comment)))
  "Face for comments.")

(defface racket-module-face
  `((t (:foreground ,racket-colors-blue)))
  "Face for module names (require/provide/etc.).")

;; -----------------------------
;; Font-lock keywords
;; -----------------------------
(setq racket-colors--keywords
      `(
        ;; Keywords
        (,(regexp-opt '("define" "lambda" "let" "let*" "letrec" "if" "cond" "match"
                        "begin" "require" "provide" "struct" "set!" "quote" "quasiquote")
                      'symbols)
         . 'racket-keyword-face)

        ;; Booleans
        ("#t\\|#f" . 'racket-boolean-face)

        ;; Numbers
        ("\\_<[0-9]+\\_>" . 'racket-number-face)

        ;; Strings
        ("\"[^\"]*\"" . 'racket-string-face)

        ;; Built-ins
        (,(regexp-opt '("display" "displayln" "newline" "car" "cdr" "list" "map" "for" "for-each")
                      'symbols)
         . 'racket-builtin-face)

        ;; Identifiers
        ("\\_<[A-Za-z0-9!$%&*+./:<=>?@^_~-]+\\_>" . 'racket-identifier-face)

        ;; Comments
        (";.*$" . 'racket-comment-face)
        ))

;; -----------------------------
;; Apply to Racket modes
;; -----------------------------
(defun racket-colors-apply ()
  "Enable Racket custom highlighting."
  (font-lock-add-keywords nil racket-colors--keywords 'append)
  (font-lock-flush)
  (font-lock-ensure))

(add-hook 'racket-mode-hook #'racket-colors-apply)
(add-hook 'racket-repl-mode-hook #'racket-colors-apply)

(provide 'lang-racket-colors)

;;; racket-colors.el ends here
