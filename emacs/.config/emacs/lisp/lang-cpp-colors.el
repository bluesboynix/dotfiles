;;; lang-cpp-colors.el --- Harmonized C/C++ syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; Elegant, dark-friendly, and harmonized with Lisp and Rust faces.
;; Includes distinct faces for variables, functions, and preprocessor directives.

;;; Code:

;; -------------------------------------------------------------------
;; Faces
;; -------------------------------------------------------------------

(defface c-keyword-face
  '((t (:foreground "#d3869b" :weight semi-bold))) ; lavender/purple
  "Face for C/C++ control and storage keywords.")

(defface c-type-face
  '((t (:foreground "#afd0ff"))) ; light blue
  "Face for C/C++ data types, structs, and class names.")

(defface c-function-name-face
  '((t (:foreground "#8399ff" :weight semi-bold))) ; cyan-gray
  "Face for C/C++ function definitions.")

(defface c-function-call-face
  '((t (:foreground "#90b0ff"))) ; slightly lighter blue
  "Face for C/C++ function calls.")

(defface c-variable-face
  '((t (:foreground "#c0c0c0"))) ; subtle gray-blue
  "Face for local variable names and identifiers.")

(defface c-constant-face
  '((t (:foreground "#fabd2f" :weight bold))) ; amber
  "Face for constants and macros.")

(defface c-number-face
  '((t (:foreground "#d79921"))) ; warm gold
  "Face for numeric literals.")

(defface c-string-face
  '((t (:foreground "#b8bb26"))) ; olive green
  "Face for string literals.")

(defface c-comment-face
  '((t (:foreground "#928374" :slant italic))) ; gray
  "Face for comments.")

(defface c-preprocessor-face
  '((t (:foreground "#ffaf87" :weight semi-bold))) ; coral
  "Face for preprocessor directives (#include, #define, etc.).")

;; -------------------------------------------------------------------
;; Keyword Lists
;; -------------------------------------------------------------------

(defconst c-type-keywords
  '("int" "long" "short" "char" "float" "double" "void" "bool"
    "signed" "unsigned" "size_t" "ssize_t" "auto" "struct" "enum"
    "union" "class" "typename" "decltype" "constexpr")
  "C/C++ type-related keywords.")

(defconst c-keywords
  '("if" "else" "for" "while" "do" "switch" "case" "default"
    "break" "continue" "return" "goto"
    "try" "catch" "throw" "new" "delete" "public" "private" "protected"
    "virtual" "inline" "namespace" "using" "template" "friend" "operator")
  "C/C++ control and declaration keywords.")

(defconst c-constants
  '("NULL" "nullptr" "true" "false" "stdin" "stdout" "stderr")
  "C/C++ constant literals.")

;; -------------------------------------------------------------------
;; Regex Definitions
;; -------------------------------------------------------------------

(defconst c-function-def-regexp
  "\\b\\([A-Za-z_][A-Za-z_0-9]*\\)\\s-*("
  "Regex for detecting function names (definition or call).")

(defconst c-variable-regexp
  "\\b\\([A-Za-z_][A-Za-z_0-9]*\\)\\s-*="
  "Regex for detecting variable names.")

(defconst c-number-regexp
  "\\b[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\b"
  "Regex for numeric literals.")

(defconst c-preprocessor-regexp
  "^\\s-*#\\s-*\\(include\\|define\\|ifdef\\|ifndef\\|endif\\|pragma\\)"
  "Regex for C/C++ preprocessor directives.")

;; -------------------------------------------------------------------
;; Apply Custom Highlighting
;; -------------------------------------------------------------------

(defun c-cpp-apply-custom-highlighting ()
  "Apply custom font-lock rules for C and C++."
  (font-lock-add-keywords
   nil
   `(
     ;; Keyword groups
     (,(regexp-opt c-type-keywords 'words) . 'c-type-face)
     (,(regexp-opt c-keywords 'words)      . 'c-keyword-face)
     (,(regexp-opt c-constants 'words)     . 'c-constant-face)
     ;; Function names (definitions/calls)
     (,c-function-def-regexp 1 'c-function-call-face)
     ;; Variable assignments
     (,c-variable-regexp 1 'c-variable-face)
     ;; Numbers and strings
     (,c-number-regexp                    . 'c-number-face)
     ("\"\\([^\"\\]\\|\\\\.\\)*\""        . 'c-string-face)
     ;; Comments
     ("//.*$"                             . 'c-comment-face)
     ("/\\*\\(.\\|\n\\)*?\\*/"            . 'c-comment-face)
     ;; Preprocessor
     (,c-preprocessor-regexp              . 'c-preprocessor-face))))

(add-hook 'c-mode-hook #'c-cpp-apply-custom-highlighting)
(add-hook 'c++-mode-hook #'c-cpp-apply-custom-highlighting)

(provide 'lang-cpp-colors)
;;; lang-cpp-colors.el ends here
