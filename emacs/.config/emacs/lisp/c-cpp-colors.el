;;; c-cpp-colors.el --- Harmonized C/C++ syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified aesthetic with Common Lisp highlighting.
;; Designed for elegant, symbolic, and dark-friendly visuals.
;; All faces use regular weight for a smooth, balanced look.

;;; Code:

;; -------------------------------------------------------------------
;; Faces (aligned with common-lisp-colors.el)
;; -------------------------------------------------------------------

(defface c-keyword-face
  '((t (:foreground "#d3869b"))) ; lavender/purple — control flow
  "Face for C/C++ control and storage keywords.")

(defface c-type-face
  '((t (:foreground "#8ec07c"))) ; green — types, structs
  "Face for C/C++ data types and class keywords.")

(defface c-function-name-face
  '((t (:foreground "#83a598"))) ; cyan-gray — function names
  "Face for C/C++ function names in definitions or calls.")

(defface c-constant-face
  '((t (:foreground "#fabd2f"))) ; amber — constants
  "Face for C/C++ constants and macros.")

(defface c-number-face
  '((t (:foreground "#d79921"))) ; warm gold — numeric literals
  "Face for C/C++ numeric literals.")

(defface c-string-face
  '((t (:foreground "#b8bb26"))) ; olive green — strings
  "Face for C/C++ string literals.")

(defface c-comment-face
  '((t (:foreground "#928374" :slant italic))) ; gray — comments
  "Face for comments.")

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
  "Regex for detecting function calls/definitions.")

(defconst c-number-regexp
  "\\b[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\b"
  "Regex for detecting numbers.")

;; -------------------------------------------------------------------
;; Apply Custom Highlighting
;; -------------------------------------------------------------------

(defun c-cpp-apply-custom-highlighting ()
  "Apply custom font-lock rules for C and C++."
  (font-lock-add-keywords
   nil
   `(
     (,(regexp-opt c-type-keywords 'words) . 'c-type-face)
     (,(regexp-opt c-keywords 'words)      . 'c-keyword-face)
     (,(regexp-opt c-constants 'words)     . 'c-constant-face)
     (,c-function-def-regexp 1             'c-function-name-face)
     (,c-number-regexp                    . 'c-number-face)
     ("\".*?\""                           . 'c-string-face)
     ("//.*$"                             . 'c-comment-face)
     ("/\\*\\(.\\|\n\\)*?\\*/"            . 'c-comment-face))))

(add-hook 'c-mode-hook #'c-cpp-apply-custom-highlighting)
(add-hook 'c++-mode-hook #'c-cpp-apply-custom-highlighting)

(provide 'c-cpp-colors)
;;; c-cpp-colors.el ends here
