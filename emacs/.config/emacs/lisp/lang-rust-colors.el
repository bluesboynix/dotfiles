;;; lang-rust-colors.el --- Extended Rust syntax colors -*- lexical-binding: t; -*-

;;; Commentary:
;; Deep Rust-specific font-lock highlighting.
;; Designed for Genesis Dark–style themes.
;; Works with rust-mode and rust-ts-mode.

;;; Code:

;; --------------------------------------------------
;; Face definitions
;; --------------------------------------------------

(defface rust-keyword-face
  '((t (:foreground "#0fa0ff" :weight semi-bold)))
  "Rust keywords.")

(defface rust-type-face
  '((t (:foreground "#b0c8ff")))
  "Rust types, traits, structs.")

(defface rust-primitive-face
  '((t (:foreground "#89ddff")))
  "Primitive Rust types.")

(defface rust-bool-face
  '((t (:foreground "#d19a66")))
  "Boolean literals.")

(defface rust-number-face
  '((t (:foreground "#e5c07b")))
  "Numeric literals.")

(defface rust-string-face
  '((t (:foreground "#98c379")))
  "String and byte string literals.")

(defface rust-char-face
  '((t (:foreground "#7ee787")))
  "Character literals.")

(defface rust-function-face
  '((t (:foreground "#ffffaa")))
  "Functions and methods.")

(defface rust-macro-face
  '((t (:foreground "#ffaa0f")))
  "Macros and macro_rules.")

(defface rust-variable-face
  '((t (:foreground "#aab2c0")))
  "Variables and bindings.")

(defface rust-constant-face
  '((t (:foreground "#ffcb6b")))
  "Constants and enum variants.")

(defface rust-lifetime-face
  '((t (:foreground "#e06c75")))
  "Lifetimes.")

(defface rust-attribute-face
  '((t (:foreground "#7f848e")))
  "Attributes and derives.")

(defface rust-path-face
  '((t (:foreground "#82aaff")))
  "Module and type paths.")

(defface rust-doc-face
  '((t (:foreground "#5c6370" :slant italic)))
  "Documentation comments.")

(defface rust-comment-face
  '((t (:foreground "#5c6370" :slant italic)))
  "Regular comments.")

;; --------------------------------------------------
;; Font-lock rules
;; --------------------------------------------------

(defun my-rust-font-lock ()
  "Extended Rust syntax highlighting."
  (font-lock-add-keywords
   nil
   `(
     ;; Keywords
     (,(regexp-opt
        '("fn" "let" "mut" "if" "else" "match" "loop" "while" "for"
          "in" "impl" "trait" "struct" "enum" "use" "mod" "pub"
          "crate" "super" "self" "Self" "return" "break" "continue"
          "async" "await" "move" "ref" "dyn" "as" "const" "static"
          "unsafe" "where" "extern" "type" "macro_rules!")
        'symbols)
      0 'rust-keyword-face)

     ;; Visibility qualifiers
     ("\\bpub(\\s-*\\((crate|super|self)\\))?" 0 'rust-keyword-face)

     ;; Function definitions
     ("\\bfn\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 'rust-function-face)

     ;; Function calls / methods
     ("\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 'rust-function-face)

     ;; Variables after let / mut
     ("\\b\\(?:let\\|mut\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
      1 'rust-variable-face)

     ;; Types / Traits / Structs
     ("\\b[A-Z][A-Za-z0-9_]*\\b" 0 'rust-type-face)

     ;; Enum variants (Type::Variant)
     ("\\b[A-Z][A-Za-z0-9_]*::\\([A-Z][A-Za-z0-9_]*\\)"
      1 'rust-constant-face)

     ;; Paths (module::item)
     ("\\b\\([a-z][A-Za-z0-9_]*::\\)+" 0 'rust-path-face)

     ;; Primitive types
     ("\\b\\(u8\\|u16\\|u32\\|u64\\|usize\\|i8\\|i16\\|i32\\|i64\\|isize\\|f32\\|f64\\|bool\\|char\\|str\\)\\b"
      0 'rust-primitive-face)

     ;; Booleans
     ("\\b\\(true\\|false\\)\\b" 0 'rust-bool-face)

     ;; Numbers with suffixes
     ("\\b[0-9]+\\(\\.[0-9]+\\)?\\([iu][0-9]+\\|f[0-9]+\\)?\\b"
      0 'rust-number-face)

     ;; Strings (normal, raw, byte)
     ("b?\"\\([^\"\\\\]\\|\\\\.\\)*\"" 0 'rust-string-face)
     ("r#*\".*?\"#*" 0 'rust-string-face)

     ;; Character literals
     ("'\\(\\\\.\\|[^']\\)'" 0 'rust-char-face)

     ;; Macros
     ("\\b[A-Za-z_][A-Za-z0-9_]*!" 0 'rust-macro-face)

     ;; Lifetimes
     ("'\\(static\\|[a-zA-Z_][A-Za-z0-9_]*\\)" 0 'rust-lifetime-face)

     ;; Attributes
     ("#\\!?\\[.*?\\]" 0 'rust-attribute-face)

     ;; Doc comments
     ("///.*" 0 'rust-doc-face)
     ("//!.*" 0 'rust-doc-face)

     ;; Regular comments
     ("//.*" 0 'rust-comment-face)
     )))

;; --------------------------------------------------
;; Activation
;; --------------------------------------------------

(defun my-rust-activate-custom-faces ()
  "Enable extended Rust font-lock."
  (my-rust-font-lock)
  (font-lock-flush)
  (font-lock-ensure))

(add-hook 'rust-mode-hook #'my-rust-activate-custom-faces)
(add-hook 'rust-ts-mode-hook #'my-rust-activate-custom-faces)

(provide 'lang-rust-colors)
;;; lang-rust-colors.el ends here
