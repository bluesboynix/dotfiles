;;; lang-rust-colors.el --- Custom Rust syntax colors -*- lexical-binding: t; -*-
;;; Commentary:
;; Enhanced font-lock highlighting for Rust code.
;; Works seamlessly with the Genesis Dark theme.
;;; Code:

;; ------------------------------
;; Face definitions
;; ------------------------------

(defface rust-keyword-face
  '((t (:foreground "#0fa0ff" :weight semi-bold))) ;; cyan-blue
  "Face for Rust keywords.")

(defface rust-type-face
  '((t (:foreground "#b0c8ff"))) ;; light blue
  "Face for Rust types and traits.")

(defface rust-primitive-face
  '((t (:foreground "#89ddff"))) ;; cyan
  "Face for Rust primitive types (u8, i32, etc.).")

(defface rust-bool-face
  '((t (:foreground "#d19a66"))) ;; brownish-orange
  "Face for booleans true/false.")

(defface rust-number-face
  '((t (:foreground "#e5c07b"))) ;; amber
  "Face for numeric literals.")

(defface rust-string-face
  '((t (:foreground "#98c379"))) ;; green
  "Face for string literals.")

(defface rust-function-face
  '((t (:foreground "#ffffaa"))) ;; pale yellow
  "Face for function names and calls.")

(defface rust-variable-face
  '((t (:foreground "#aab2c0"))) ;; soft gray-blue
  "Face for variable names after `let` or `mut`.")

(defface rust-constant-face
  '((t (:foreground "#ffcb6b"))) ;; yellow
  "Face for constants and enums.")

(defface rust-macro-face
  '((t (:foreground "#ffaa0f"))) ;; orange
  "Face for macros (println!, etc.).")

(defface rust-lifetime-face
  '((t (:foreground "#e06c75"))) ;; soft red
  "Face for lifetimes ('a, etc.).")

(defface rust-attribute-face
  '((t (:foreground "#7f848e"))) ;; gray
  "Face for attributes #[derive(...)]")

(defface rust-comment-face
  '((t (:foreground "#5c6370" :slant italic))) ;; muted gray italic
  "Face for comments.")

;; ------------------------------
;; Font-lock keywords
;; ------------------------------

(defun my-rust-font-lock ()
  "Enhanced syntax highlighting for Rust, including function defs, calls, and vars."
  (font-lock-add-keywords
   nil
   `(
     ;; Keywords
     (,(regexp-opt
        '("fn" "let" "mut" "if" "else" "match" "loop" "while" "for"
          "in" "impl" "trait" "struct" "enum" "use" "mod" "pub"
          "crate" "super" "self" "return" "break" "continue"
          "async" "await" "move" "ref" "dyn" "as" "const" "static"
          "unsafe" "where" "extern" "type")
        'symbols)
      0 'rust-keyword-face)

     ;; Function definitions — highlight name after `fn`
     ("\\bfn[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
      1 'rust-function-face)

     ;; Function calls — identifier followed by (
     ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*("
      1 'rust-function-face)

     ;; Variable names after `let` or `mut`
     ("\\b\\(?:let\\|mut\\)[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
      1 'rust-variable-face)

     ;; Types and Traits (start with uppercase)
     ("\\b[A-Z][A-Za-z0-9_]*\\b" 0 'rust-type-face)

     ;; Primitive types
     ("\\b\\(u8\\|u16\\|u32\\|u64\\|usize\\|i8\\|i16\\|i32\\|i64\\|isize\\|f32\\|f64\\)\\b"
      0 'rust-primitive-face)

     ;; Booleans
     ("\\b\\(true\\|false\\)\\b" 0 'rust-bool-face)

     ;; Numbers (integer and float)
     ("\\b[0-9]+\\(\\.[0-9]+\\)?\\b" 0 'rust-number-face)

     ;; Strings
     ("\"[^\"]*\"" 0 'rust-string-face)

     ;; Macros (ending with !)
     ("\\b[a-zA-Z_][a-zA-Z0-9_]*!" 0 'rust-macro-face)

     ;; Lifetimes ('a, 'static)
     ("'\\sw+" 0 'rust-lifetime-face)

     ;; Attributes (#[derive(...)] etc.)
     ("#\\[.*?\\]" 0 'rust-attribute-face)

     ;; Comments (// ... and /// ...)
     ("//.*" 0 'rust-comment-face)
     )))

;; ------------------------------
;; Hook integration
;; ------------------------------

(defun my-rust-activate-custom-faces ()
  "Activate custom Rust faces and refresh font-lock after theme load."
  (my-rust-font-lock)
  (font-lock-flush)
  (font-lock-ensure))

(add-hook 'rust-mode-hook #'my-rust-activate-custom-faces)

(provide 'lang-rust-colors)
;;; lang-rust-colors.el ends here
