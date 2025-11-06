;;; lang-python-colors.el --- Custom Python syntax highlighting -*- lexical-binding: t; -*-
;;; Commentary:
;; Aesthetic, calm color scheme for Python code.
;; Harmonized with C/C++ and Rust color palettes.
;; Designed for readability and symbolic clarity.

;;; Code:

;; -------------------------------------------------------------------
;; Faces
;; -------------------------------------------------------------------

(defface python-keyword-face
  '((t (:foreground "#d3869b" :weight semi-bold))) ; lavender/purple
  "Face for Python keywords like if, else, def, return.")

(defface python-builtin-face
  '((t (:foreground "#7daea3"))) ; muted teal
  "Face for Python built-in functions and exceptions.")

(defface python-function-name-face
  '((t (:foreground "#8399ff" :weight semi-bold))) ; soft blue
  "Face for Python function definitions and calls.")

(defface python-variable-face
  '((t (:foreground "#a9b1d6"))) ; soft gray-blue
  "Face for local variables and identifiers.")

(defface python-constant-face
  '((t (:foreground "#fabd2f" :weight semi-bold))) ; amber/gold
  "Face for Python constants like True, False, None.")

(defface python-number-face
  '((t (:foreground "#d79921"))) ; warm gold
  "Face for Python numeric literals.")

(defface python-string-face
  '((t (:foreground "#b8bb26"))) ; olive green
  "Face for Python string literals.")

(defface python-comment-face
  '((t (:foreground "#928374" :slant italic))) ; gray
  "Face for Python comments.")

(defface python-decorator-face
  '((t (:foreground "#fe8019"))) ; orange
  "Face for Python decorators (@decorator).")

;; -------------------------------------------------------------------
;; Keyword Lists
;; -------------------------------------------------------------------

(defconst python-keywords
  '("and" "as" "assert" "break" "class" "continue" "def" "del" "elif" "else" "except"
    "False" "finally" "for" "from" "global" "if" "import" "in" "is" "lambda"
    "None" "nonlocal" "not" "or" "pass" "raise" "return" "True" "try" "while" "with" "yield")
  "Python keywords.")

(defconst python-builtins
  '("abs" "all" "any" "ascii" "bin" "bool" "bytearray" "bytes" "callable" "chr"
    "classmethod" "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
    "eval" "exec" "filter" "float" "format" "frozenset" "getattr" "globals"
    "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance" "issubclass"
    "iter" "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object"
    "oct" "open" "ord" "pow" "print" "property" "range" "repr" "reversed" "round"
    "set" "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super" "tuple"
    "type" "vars" "zip" "__import__")
  "Python built-in functions and classes.")

;; -------------------------------------------------------------------
;; Regex Patterns
;; -------------------------------------------------------------------

(defconst python-function-def-regexp
  "\\bdef\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
  "Regex for Python function definitions.")

(defconst python-decorator-regexp
  "@[A-Za-z_][A-Za-z0-9_\\.]*"
  "Regex for Python decorators.")

(defconst python-number-regexp
  "\\b[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\b"
  "Regex for Python numeric literals.")

;; -------------------------------------------------------------------
;; Apply Custom Highlighting
;; -------------------------------------------------------------------

(defun lang-python-apply-custom-highlighting ()
  "Apply custom syntax highlighting for Python."
  (font-lock-add-keywords
   nil
   `(
     (,(regexp-opt python-keywords 'words) . 'python-keyword-face)
     (,(regexp-opt python-builtins 'words) . 'python-builtin-face)
     (,python-function-def-regexp 1 'python-function-name-face)
     (,python-decorator-regexp . 'python-decorator-face)
     (,python-number-regexp . 'python-number-face)
     ("\\_<[A-Za-z_][A-Za-z0-9_]*\\_>" . 'python-variable-face)
     ("\"\"\"[\\s\\S]*?\"\"\"" . 'python-string-face) ; triple double-quoted
     ("'''[\\s\\S]*?'''" . 'python-string-face)       ; triple single-quoted
     ("\"[^\"]*\"" . 'python-string-face)             ; double-quoted
     ("'[^']*'" . 'python-string-face)                ; single-quoted
     ("#.*$" . 'python-comment-face)
     )))

(add-hook 'python-mode-hook #'lang-python-apply-custom-highlighting)

(provide 'lang-python-colors)
;;; lang-python-colors.el ends here
