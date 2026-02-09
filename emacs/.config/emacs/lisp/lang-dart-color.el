;;; lang-dart-color.el --- Dart explicit colors + stdlib -*- lexical-binding: t; -*-

;;; Commentary:
;; Dart syntax highlighting with fixed colors.
;; Includes standard library symbols and dart: imports.

;;; Code:

(require 'font-lock)

;; ------------------------------
;; Keyword groups
;; ------------------------------

(defconst dart-keywords
  '("abstract" "as" "assert" "async" "await"
    "break" "case" "catch" "class" "const" "continue"
    "default" "do"
    "else" "enum" "export" "extends"
    "final" "finally" "for"
    "if" "implements" "import" "in" "interface" "is"
    "late"
    "mixin"
    "new" "null"
    "on" "operator"
    "return" "rethrow"
    "static" "super" "switch"
    "this" "throw" "true" "try"
    "var" "void" "while" "yield"))

(defconst dart-types
  '("int" "double" "num" "bool" "String"
    "List" "Map" "Set" "Iterable"
    "Future" "Stream" "Object"
    "dynamic" "Never" "Type"))

;; ------------------------------
;; Dart standard library symbols
;; ------------------------------

(defconst dart-stdlib-symbols
  '(
    ;; Core functions
    "print" "identical" "assert"

    ;; Core classes
    "Duration" "DateTime" "Stopwatch" "RegExp"
    "Uri" "Symbol" "BigInt"

    ;; Collections
    "List" "Map" "Set" "Queue" "Iterable" "Iterator"

    ;; Async
    "Future" "Stream" "StreamController" "Completer"

    ;; Math / num
    "num" "int" "double"

    ;; Errors
    "Exception" "Error" "StateError" "ArgumentError"
    ))

;; ------------------------------
;; Explicit faces
;; ------------------------------

(defface dart-keyword-face
  '((t (:foreground "#ff6c6b" :weight bold)))
  "Dart keywords.")

(defface dart-type-face
  '((t (:foreground "#ECBE7B")))
  "Dart built-in types.")

(defface dart-stdlib-face
  '((t (:foreground "#46d9ff" :weight bold)))
  "Dart standard library symbols.")

(defface dart-constant-face
  '((t (:foreground "#98be65" :weight bold)))
  "Dart constants.")

(defface dart-annotation-face
  '((t (:foreground "#51afef" :slant italic)))
  "Dart annotations.")

(defface dart-function-face
  '((t (:foreground "#c678dd")))
  "Dart function names.")

;; ------------------------------
;; Font-lock rules
;; ------------------------------

(defconst dart-font-lock-rules
  `(
    ;; dart:xxx imports
    ("import[ \t]+['\"]\\(dart:[^'\"]+\\)['\"]"
     1 dart-stdlib-face)

    ;; Keywords
    (,(regexp-opt dart-keywords 'symbols) . dart-keyword-face)

    ;; Types
    (,(regexp-opt dart-types 'symbols) . dart-type-face)

    ;; Standard library symbols
    (,(regexp-opt dart-stdlib-symbols 'symbols) . dart-stdlib-face)

    ;; Constants
    ("\\_<\\(true\\|false\\|null\\)\\_>" . dart-constant-face)

    ;; Annotations
    ("@[A-Za-z_][A-Za-z0-9_]*" . dart-annotation-face)

    ;; Function names
    ("\\_<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\_>[ \t\n]*("
     1 dart-function-face)
    ))

;; ------------------------------
;; Apply rules
;; ------------------------------

(defun dart-apply-explicit-colors ()
  "Apply explicit Dart colors including stdlib."
  (font-lock-add-keywords nil dart-font-lock-rules 'append)
  (font-lock-flush)
  (font-lock-ensure))

(add-hook 'dart-mode-hook #'dart-apply-explicit-colors)
(add-hook 'flutter-mode-hook #'dart-apply-explicit-colors)

(provide 'lang-dart-color)

;;; lang-dart-color.el ends here
