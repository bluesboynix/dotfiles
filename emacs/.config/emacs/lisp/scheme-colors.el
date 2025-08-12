;;; scheme-colors.el --- Rainbow delimiters for Scheme -*- lexical-binding: t; -*-

(setq font-lock-maximum-decoration t)

;; Define our custom faces
(defface scheme-keyword-face
  '((t :foreground "#ff6c6b" :weight bold))
  "Face for Scheme keywords (define, lambda, etc.).")

(defface scheme-function-face
  '((t :foreground "#51afef"))
  "Face for Scheme functions and special forms.")

(defface scheme-variable-face
  '((t :foreground "#c678dd"))
  "Face for Scheme variables.")

(defface scheme-constant-face
  '((t :foreground "#98be65"))
  "Face for constants like #t, #f, numbers.")

(defface scheme-string-face
  '((t :foreground "#da8548"))
  "Face for strings.")

(defface scheme-comment-face
  '((t :foreground "#5B6268" :slant italic))
  "Face for comments.")

;; Font-lock rules
(defvar scheme-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt '("define" "lambda" "if" "cond" "else" "let" "let*" "letrec" "begin" "set!" "quote") 'symbols)
     . 'scheme-keyword-face)

    ;; Booleans and numbers
    ("#[tf]" . 'scheme-constant-face)
    ("\\_<[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . 'scheme-constant-face)

    ;; Strings
    ("\".*?\"" . 'scheme-string-face)

    ;; Comments
    (";.*" . 'scheme-comment-face)
    ))

(defun scheme-colors-setup ()
  "Apply custom syntax highlighting for Scheme."
  (font-lock-add-keywords nil scheme-font-lock-keywords))

(add-hook 'scheme-mode-hook #'scheme-colors-setup)


(defun my-scheme-rainbow-setup ()
  "Enable rainbow-delimiters with custom colors for Scheme."
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode 1)
  ;; Apply custom colors (overrides theme)
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
