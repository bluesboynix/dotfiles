;;; lang-cpp-colors.el --- Full-color major mode for C/C++ -*- lexical-binding: t -*-
;;; Commentary:
;; A standalone Emacs major mode providing rich color and font-lock highlighting for C and C++.
;; Includes detailed coloring for STL types, macros, operators, namespaces, preprocessor directives, and more.
;; Drop this file into your load-path and add:
;;   (require 'lang-cpp-colors)
;;   (add-to-list 'auto-mode-alist '("\\.c\\'" . lang-cpp-colors-mode))
;;   (add-to-list 'auto-mode-alist '("\\.cpp\\'" . lang-cpp-colors-mode))
;;
;;; Code:

(defgroup lang-cpp-colors nil
  "Enhanced syntax coloring for C and C++." :group 'faces)

;; Faces
(defface lang-cpp-colors-keyword-face '((t :weight bold :foreground "#ff6"))
  "Face for C/C++ keywords." :group 'lang-cpp-colors)

(defface lang-cpp-colors-type-face '((t :foreground "#61afef" :weight bold))
  "Face for type names (built-ins and STL types)." :group 'lang-cpp-colors)

(defface lang-cpp-colors-function-face '((t :foreground "#56b6c2" :weight bold))
  "Face for function names." :group 'lang-cpp-colors)

(defface lang-cpp-colors-constant-face '((t :foreground "#d19a66" :weight bold))
  "Face for constants and enums." :group 'lang-cpp-colors)

(defface lang-cpp-colors-macro-face '((t :foreground "#e5c07b" :weight bold))
  "Face for macros." :group 'lang-cpp-colors)

(defface lang-cpp-colors-number-face '((t :foreground "#c678dd"))
  "Face for numeric literals." :group 'lang-cpp-colors)

(defface lang-cpp-colors-operator-face '((t :foreground "#98c379" :weight bold))
  "Face for operators." :group 'lang-cpp-colors)

(defface lang-cpp-colors-preprocessor-face '((t :foreground "#be5046" :weight bold))
  "Face for preprocessor directives." :group 'lang-cpp-colors)

(defface lang-cpp-colors-member-face '((t :slant italic :foreground "#c678dd"))
  "Face for struct/class members." :group 'lang-cpp-colors)

(defface lang-cpp-colors-namespace-face '((t :foreground "#61afef" :weight bold))
  "Face for namespaces." :group 'lang-cpp-colors)

;; Extended STL and built-in lists
(defvar lang-cpp-colors--stl-types
  (regexp-opt '("std" "vector" "string" "wstring" "list" "deque" "queue" "priority_queue"
                 "stack" "set" "unordered_set" "map" "unordered_map" "multiset"
                 "multimap" "array" "bitset" "tuple" "pair" "span" "optional"
                 "variant" "any" "filesystem" "thread" "mutex" "atomic" "shared_ptr"
                 "unique_ptr" "weak_ptr" "function" "regex" "ostream" "istream"
                 "stringstream" "ifstream" "ofstream" "iostream"))
  "STL types for highlighting.")

(defvar lang-cpp-colors--builtin-types
  (regexp-opt '("int" "long" "short" "char" "signed" "unsigned" "float" "double" "void" "bool"
                 "size_t" "ssize_t" "auto" "decltype" "constexpr"))
  "Built-in C/C++ types.")

(defvar lang-cpp-colors--keywords
  (regexp-opt '("if" "else" "switch" "case" "for" "while" "do" "break" "continue" "return"
                 "goto" "try" "catch" "throw" "using" "namespace" "class" "struct" "union"
                 "public" "private" "protected" "virtual" "override" "final" "constexpr"
                 "consteval" "constinit" "const" "volatile" "inline" "static" "extern"
                 "register" "template" "typename" "new" "delete" "operator" "this" "friend"
                 "enum"))
  "C/C++ keywords.")

(defvar lang-cpp-colors--boolean-const (regexp-opt '("true" "false" "NULL" "nullptr")))

;; Font-lock keywords
(defvar lang-cpp-colors-font-lock-keywords
  `(
    ("^\\s-*#\\s-*\\(include\\|if\\|ifdef\\|ifndef\\|elif\\|else\\|endif\\|define\\|undef\\|pragma\\)\\b" . 'lang-cpp-colors-preprocessor-face)
    ("#\\s-*include\\s-*<\\([^>]+\\)>" 1 'lang-cpp-colors-constant-face)
    ("#\\s-*include\\s-*\"\\([^\"]+\\)\"" 1 'lang-cpp-colors-constant-face)
    (,(concat "\\<" lang-cpp-colors--keywords "\\>") . 'lang-cpp-colors-keyword-face)
    (,(concat "\\<" lang-cpp-colors--builtin-types "\\>") . 'lang-cpp-colors-type-face)
    (,(concat "\\(?:std::\\)?\\(" lang-cpp-colors--stl-types "\\)\\>") 1 'lang-cpp-colors-type-face)
    ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*::" 1 'lang-cpp-colors-namespace-face)
    ("\\(?:->\\|\\.\\)\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 'lang-cpp-colors-member-face)
    ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>" 1 'lang-cpp-colors-constant-face)
    (,(concat "\\<" lang-cpp-colors--boolean-const "\\>") . 'lang-cpp-colors-constant-face)
    ("\\b0[xX][0-9A-Fa-f]+\\b" . 'lang-cpp-colors-number-face)
    ("\\b[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?[uUlLfF]*\\b" . 'lang-cpp-colors-number-face)
    ("\\(->\\|::\\|&&\\|||\\|==\\|!=\\|<=\\|>=\\|\\+=\\|-==\\|\\*=\\|/=\\|%=\\|<<\\|>>\\)" . 'lang-cpp-colors-operator-face)
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*\\(([^)]*)\\)" 1 'lang-cpp-colors-function-face))
  "Font-lock highlighting expressions for lang-cpp-colors mode.")

;;;###autoload
(define-derived-mode lang-cpp-colors-mode prog-mode "Lang-CPP-Colors"
  "Major mode for colorful C/C++ highlighting."
  (setq font-lock-defaults '(lang-cpp-colors-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.c\\'" . lang-cpp-colors-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . lang-cpp-colors-mode))

(provide 'lang-cpp-colors)
;;; lang-cpp-colors.el ends here
