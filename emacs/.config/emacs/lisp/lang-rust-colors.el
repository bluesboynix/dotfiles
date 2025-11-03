;;; lang-rust-colors.el --- Custom Rust syntax colors -*- lexical-binding: t; -*-
;;; Commentary:
;; Extra font-lock highlighting for Rust code.
;;; Code:

(defface rust-keyword-face
  '((t (:foreground "#c792ea"))) ;; purple
  "Face for Rust keywords.")

(defface rust-type-face
  '((t (:foreground "#82aaff"))) ;; blue
  "Face for Rust types and traits.")

(defface rust-primitive-face
  '((t (:foreground "#89ddff"))) ;; cyan
  "Face for Rust primitive types.")

(defface rust-bool-face
  '((t (:foreground "#d19a66"))) ;; brownish-orange
  "Face for booleans true/false.")

(defface rust-number-face
  '((t (:foreground "#e5c07b"))) ;; amber
  "Face for numbers.")

(defface rust-string-face
  '((t (:foreground "#98c379"))) ;; green
  "Face for string literals.")

(defface rust-function-face
  '((t (:foreground "#61afef"))) ;; blue
  "Face for function names.")

(defface rust-constant-face
  '((t (:foreground "#ffcb6b"))) ;; yellow
  "Face for constants.")

(defface rust-macro-face
  '((t (:foreground "#ff5370"))) ;; pink-red
  "Face for macros (println!, etc.)")

(defface rust-lifetime-face
  '((t (:foreground "#e06c75"))) ;; soft red
  "Face for lifetimes ('a, etc.)")

(defface rust-attribute-face
  '((t (:foreground "#7f848e"))) ;; gray
  "Face for attributes #[derive(...)]")

(defface rust-comment-face
  '((t (:foreground "#5c6370" :slant italic))) ;; muted gray italic
  "Face for comments.")

(defun my-rust-font-lock ()
  "Enhanced syntax highlighting for Rust."
  (font-lock-add-keywords
   nil
   `((,(regexp-opt
        '("fn" "let" "mut" "if" "else" "match" "loop" "while" "for"
          "in" "impl" "trait" "struct" "enum" "use" "mod" "pub"
          "crate" "super" "self" "return" "break" "continue"
          "async" "await" "move" "ref" "dyn" "as" "const" "static"
          "unsafe" "where" "extern" "type")
        'symbols)
      0 'rust-keyword-face)

     ("\\b[[:upper:]][[:alnum:]_]*\\b" 0 'rust-type-face)
     ("\\b\\(u8\\|u16\\|u32\\|u64\\|usize\\|i8\\|i16\\|i32\\|i64\\|isize\\|f32\\|f64\\)\\b"
      0 'rust-primitive-face)
     ("\\b\\(true\\|false\\)\\b" 0 'rust-bool-face)
     ("\\b[0-9]+\\(\\.[0-9]+\\)?\\b" 0 'rust-number-face)
     ("\"[^\"]*\"" 0 'rust-string-face)
     ("\\b[a-zA-Z_][a-zA-Z0-9_]*!" 0 'rust-macro-face)
     ("'\\sw+" 0 'rust-lifetime-face)
     ("#\\[.*?\\]" 0 'rust-attribute-face)
     ("//.*" 0 'rust-comment-face))))

(add-hook 'rust-mode-hook #'my-rust-font-lock)

(provide 'lang-rust-colors)
;;; lang-rust-colors.el ends here
