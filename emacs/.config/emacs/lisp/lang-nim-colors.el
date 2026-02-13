;;; lang-nim-colors.el --- Custom syntax highlighting for Nim -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom font-lock keywords and faces for Nim language.
;; Load this after nim-mode to enhance syntax highlighting.

;;; Code:

;; -----------------------------
;; Custom faces for Nim
;; -----------------------------
(defface nim-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Nim keywords."
  :group 'nim-faces)

(defface nim-type-face
  '((t :inherit font-lock-type-face))
  "Face for Nim built-in types."
  :group 'nim-faces)

(defface nim-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for Nim function definitions."
  :group 'nim-faces)

(defface nim-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Nim special variables."
  :group 'nim-faces)

(defface nim-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for Nim constants and numbers."
  :group 'nim-faces)

(defface nim-field-face
  '((t :inherit font-lock-constant-face))
  "Face for Nim object fields."
  :group 'nim-faces)

(defface nim-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for Nim comments."
  :group 'nim-faces)

(defface nim-string-face
  '((t :inherit font-lock-string-face))
  "Face for Nim strings."
  :group 'nim-faces)

;; -----------------------------
;; Enhanced syntax highlighting rules
;; -----------------------------
(defvar nim-enhanced-font-lock-keywords
  (list
   ;; Keywords
   (list
    (concat "\\<\\("
            (regexp-opt
             '("addr" "and" "as" "asm" "atomic" "bind" "block" "break"
               "case" "cast" "concept" "const" "continue" "converter"
               "defer" "discard" "distinct" "div" "do" "elif" "else"
               "end" "enum" "except" "export" "finally" "for" "from"
               "func" "if" "import" "in" "include" "interface" "is"
               "isnot" "iterator" "let" "macro" "method" "mod" "nil"
               "not" "notin" "object" "of" "or" "out" "proc" "ptr"
               "raise" "ref" "return" "shl" "shr" "static" "template"
               "try" "tuple" "type" "using" "var" "when" "while"
               "with" "without" "xor" "yield"))
            "\\)\\>")
    0 'nim-keyword-face t)
   
   ;; Built-in types
   (list
    (concat "\\<\\("
            (regexp-opt
             '("int" "int8" "int16" "int32" "int64"
               "uint" "uint8" "uint16" "uint32" "uint64"
               "float" "float32" "float64"
               "bool" "char" "string" "cstring"
               "pointer" "typedesc" "void" "auto" "any"
               "untyped" "typed" "seq" "array" "openarray"
               "range" "set" "byte" "Natural" "Positive"))
            "\\)\\>")
    0 'nim-type-face t)
   
   ;; Special variables
   '("\\<\\(result\\|self\\)\\>" 0 'nim-variable-face t)
   
   ;; Function/proc definitions
   '("\\<\\(proc\\|func\\|method\\|iterator\\|template\\|macro\\)\\s-+\\([[:alnum:]_]+\\)"
     (1 'nim-keyword-face)
     (2 'nim-function-face))
   
   ;; Type definitions
   '("\\<\\(type\\|var\\|let\\|const\\)\\s-+\\([[:alnum:]_]+\\)"
     (1 'nim-keyword-face)
     (2 'nim-type-face))
   
   ;; Object fields (after dot)
   '("\\.\\([[:alnum:]_]+\\)" 1 'nim-field-face)
   
   ;; Comments (discouraging comments, ensuring they're highlighted)
   '("\\(#.*\\)" 1 'nim-comment-face t)
   
   ;; String literals
   '("\".*?\"" 0 'nim-string-face)
   
   ;; Multi-line strings (triple quotes)
   '("\"\"\".*?\"\"\"" 0 'nim-string-face)
   
   ;; Character literals - FIXED: use list instead of quoted string
   (list "'." 0 'nim-string-face)
   
   ;; Numbers
   '("\\<[0-9]+\\>" 0 'nim-constant-face)
   '("\\<0[xX][0-9a-fA-F]+\\>" 0 'nim-constant-face)  ; hex
   '("\\<0[bB][01]+\\>" 0 'nim-constant-face)         ; binary
   '("\\<0[oO][0-7]+\\>" 0 'nim-constant-face)        ; octal
   
   ;; Operators (optional)
   '("\\<\\(and\\|or\\|not\\|xor\\|shl\\|shr\\|div\\|mod\\)\\>"
     0 'nim-keyword-face))
  "Enhanced font-lock keywords for Nim mode.")

;; -----------------------------
;; Function to apply enhanced highlighting
;; -----------------------------
;;;###autoload
(defun lang-nim-enable-colors ()
  "Enable enhanced syntax highlighting for Nim mode."
  (interactive)
  (when (eq major-mode 'nim-mode)
    (font-lock-add-keywords nil nim-enhanced-font-lock-keywords 'append)
    (font-lock-flush)))

;;;###autoload
(defun lang-nim-disable-colors ()
  "Disable enhanced syntax highlighting for Nim mode."
  (interactive)
  (when (eq major-mode 'nim-mode)
    (font-lock-remove-keywords nil nim-enhanced-font-lock-keywords)
    (font-lock-flush)))

;; -----------------------------
;; Theme-specific customizations (optional)
;; -----------------------------
(defun lang-nim-apply-theme-customizations ()
  "Apply theme-specific customizations for Nim faces."
  (interactive)
  (let ((bg (face-background 'default nil t)))
    (when bg
      ;; Adjust colors based on theme background
      (cond
       ;; Dark theme adjustments
       ((< (cadddr (color-values bg)) 32768)  ; dark background
        (set-face-attribute 'nim-keyword-face nil :foreground "#ff79c6")
        (set-face-attribute 'nim-type-face nil :foreground "#8be9fd")
        (set-face-attribute 'nim-function-face nil :foreground "#50fa7b")
        (set-face-attribute 'nim-constant-face nil :foreground "#bd93f9")
        (set-face-attribute 'nim-field-face nil :foreground "#f1fa8c"))
       
       ;; Light theme adjustments
       (t
        (set-face-attribute 'nim-keyword-face nil :foreground "#d73a49")
        (set-face-attribute 'nim-type-face nil :foreground "#005cc5")
        (set-face-attribute 'nim-function-face nil :foreground "#6f42c1")
        (set-face-attribute 'nim-constant-face nil :foreground "#e36209")
        (set-face-attribute 'nim-field-face nil :foreground "#22863a"))))))

;; -----------------------------
;; Auto-enable for Nim buffers
;; -----------------------------
;;;###autoload
(define-minor-mode lang-nim-colors-mode
  "Toggle enhanced Nim syntax highlighting."
  :lighter " NimColors"
  :group 'nim
  (if lang-nim-colors-mode
      (progn
        (lang-nim-enable-colors)
        (lang-nim-apply-theme-customizations)
        (add-hook 'after-change-major-mode-hook
                  #'lang-nim-enable-colors nil t))
    (lang-nim-disable-colors)
    (remove-hook 'after-change-major-mode-hook
                 #'lang-nim-enable-colors t)))

;;;###autoload
(defun lang-nim-colors-global-enable ()
  "Enable lang-nim-colors-mode in all Nim buffers."
  (interactive)
  (add-hook 'nim-mode-hook #'lang-nim-colors-mode))

;; -----------------------------
;; Provide
;; -----------------------------
(provide 'lang-nim-colors)

;;; lang-nim-colors.el ends here
