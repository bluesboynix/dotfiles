;;; lang-scheme-colors.el --- Comprehensive Scheme syntax coloring for R5RS and R7RS
;;; Commentary:

;; This package provides comprehensive syntax highlighting for Scheme
;; with full support for R5RS and R7RS standards.

;;; Code:

(defgroup scheme-colors nil
  "Scheme syntax highlighting colors."
  :group 'faces
  :prefix "scheme-colors-")

(defface scheme-colors-keyword
  '((t :foreground "#00bfff"))
  "Face for Scheme keywords and special forms. Color: #00bfff (Deep Sky Blue)"
  :group 'scheme-colors)

(defface scheme-colors-builtin
  '((t :foreground "#4169e1"))
  "Face for Scheme built-in procedures. Color: #4169e1 (Royal Blue)"
  :group 'scheme-colors)

(defface scheme-colors-constant
  '((t :foreground "#32cd32"))
  "Face for Scheme constants. Color: #32cd32 (Lime Green)"
  :group 'scheme-colors)

(defface scheme-colors-string
  '((t :foreground "#ffd700"))
  "Face for Scheme strings. Color: #ffd700 (Gold)"
  :group 'scheme-colors)

(defface scheme-colors-comment
  '((t :foreground "#808080"))
  "Face for Scheme comments. Color: #808080 (Gray)"
  :group 'scheme-colors)

(defface scheme-colors-type
  '((t :foreground "#da70d6"))
  "Face for Scheme type definitions and declarations. Color: #da70d6 (Orchid)"
  :group 'scheme-colors)

(defface scheme-colors-function
  '((t :foreground "#00ffff"))
  "Face for function definitions. Color: #00ffff (Cyan)"
  :group 'scheme-colors)

(defface scheme-colors-variable
  '((t :foreground "#87cefa"))
  "Face for variable definitions. Color: #87cefa (Light Sky Blue)"
  :group 'scheme-colors)

(defface scheme-colors-number
  '((t :foreground "#f4a460"))
  "Face for numbers. Color: #f4a460 (Sandy Brown)"
  :group 'scheme-colors)

(defface scheme-colors-char
  '((t :foreground "#daa520"))
  "Face for character literals. Color: #daa520 (Goldenrod)"
  :group 'scheme-colors)

(defface scheme-colors-error
  '((t :foreground "#ff0000"))
  "Face for error-related syntax. Color: #ff0000 (Red)"
  :group 'scheme-colors)

(defface scheme-colors-warning
  '((t :foreground "#ffa500"))
  "Face for warning-related syntax. Color: #ffa500 (Orange)"
  :group 'scheme-colors)

(defface scheme-colors-operator
  '((t :foreground "#ff8c00"))
  "Face for S-expression operators (first element). Color: #ff8c00 (Dark Orange)"
  :group 'scheme-colors)

(defconst scheme-colors-r5rs-keywords
  '("lambda" "define" "if" "cond" "case" "and" "or" "let" "let*" "letrec"
    "begin" "do" "set!" "quote" "quasiquote" "unquote" "unquote-splicing"
    "delay" "force" "eval" "apply" "call-with-current-continuation" "call/cc"
    "values" "call-with-values" "dynamic-wind" "syntax-rules" "else" "=>"
    "define-syntax" "let-syntax" "letrec-syntax" "syntax-rules")
  "R5RS keywords and special forms.")

(defconst scheme-colors-r7rs-keywords
  '("define-record-type" "define-library" "import" "export" "include" "include-ci"
    "cond-expand" "parameterize" "guard" "unless" "when" "case-lambda"
    "let-values" "let*-values" "letrec*" "define-values" "define-record-type"
    "identifier-syntax" "syntax-error" "make-parameter" "current-input-port"
    "current-output-port" "current-error-port" "file-exists?" "delete-file"
    "command-line" "emergency-exit" "get-environment-variable"
    "get-environment-variables" "features")
  "R7RS-specific keywords and special forms.")

(defconst scheme-colors-builtin-functions
  '("cons" "car" "cdr" "caar" "cadr" "cdar" "cddr" "caaar" "caadr" "cadar" "caddr"
    "cdaar" "cdadr" "cddar" "cdddr" "list" "length" "append" "reverse" "list-tail"
    "list-ref" "memq" "memv" "member" "assq" "assv" "assoc" "list?" "null?"
    "symbol?" "symbol->string" "string->symbol" "number?" "complex?" "real?"
    "rational?" "integer?" "exact?" "inexact?" "=" "<" ">" "<=" ">=" "zero?"
    "positive?" "negative?" "odd?" "even?" "max" "min" "+" "-" "*" "/" "abs"
    "quotient" "remainder" "modulo" "gcd" "lcm" "numerator" "denominator" "floor"
    "ceiling" "truncate" "round" "rationalize" "exp" "log" "sin" "cos" "tan"
    "asin" "acos" "atan" "sqrt" "expt" "make-rectangular" "make-polar" "real-part"
    "imag-part" "magnitude" "angle" "exact->inexact" "inexact->exact" "number->string"
    "string->number" "char?" "char=?" "char<?" "char>?" "char<=?" "char>=?"
    "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?" "char-alphabetic?"
    "char-numeric?" "char-whitespace?" "char-upper-case?" "char-lower-case?"
    "char->integer" "integer->char" "char-upcase" "char-downcase" "string?"
    "make-string" "string" "string-length" "string-ref" "string-set!" "string=?"
    "string-ci=?" "string<?" "string>?" "string<=?" "string>=?" "string-ci<?"
    "string-ci>?" "string-ci<=?" "string-ci>=?" "substring" "string-append"
    "string->list" "list->string" "string-copy" "string-fill!" "vector?"
    "make-vector" "vector" "vector-length" "vector-ref" "vector-set!" "vector->list"
    "list->vector" "vector-fill!" "procedure?" "apply" "map" "for-each" "call-with-current-continuation"
    "call/cc" "values" "call-with-values" "dynamic-wind" "eval" "scheme-report-environment"
    "null-environment" "interaction-environment" "input-port?" "output-port?"
    "current-input-port" "current-output-port" "current-error-port" "with-input-from-file"
    "with-output-to-file" "call-with-input-file" "call-with-output-file" "open-input-file"
    "open-output-file" "close-input-port" "close-output-port" "read" "read-char"
    "peek-char" "eof-object?" "char-ready?" "read-line" "write" "display" "newline"
    "write-char" "load" "transcript-on" "transcript-off")
  "R5RS built-in functions.")

(defconst scheme-colors-r7rs-builtin-functions
  '("boolean=?" "symbol=?" "bytevector?" "make-bytevector" "bytevector-length"
    "bytevector-u8-ref" "bytevector-u8-set!" "bytevector-copy" "bytevector-copy!"
    "bytevector-append" "utf8->string" "string->utf8" "error" "error-object?"
    "error-object-message" "error-object-irritants" "read-error?" "file-error?"
    "get-output-bytevector" "get-output-string" "open-input-bytevector"
    "open-output-bytevector" "open-input-string" "open-output-string" "input-port-open?"
    "output-port-open?" "close-port" "read-u8" "peek-u8" "read-bytevector"
    "read-bytevector!" "read-string" "write-u8" "write-bytevector" "write-string"
    "flush-output-port" "output-port-buffer-mode" "set-current-input-port!"
    "set-current-output-port!" "set-current-error-port!" "textual-port?"
    "binary-port?" "port?" "input-port?" "output-port?" "eof-object" "exit"
    "emergency-exit" "get-environment-variable" "get-environment-variables"
    "current-second" "current-jiffy" "jiffies-per-second" "features"
    "sleep" "command-line" "digit-value" "assert" "finite?" "infinite?" "nan?"
    "square" "exact-integer-sqrt" "real-valued?" "rational-valued?" "integer-valued?"
    "div" "mod" "div-and-mod" "div0" "mod0" "div0-and-mod0" "exact" "inexact"
    "string-for-each" "string-map" "vector-for-each" "vector-map" "vector-copy"
    "vector-copy!" "vector-append" "vector-fill!" "make-list" "list-copy"
    "list-set!" "assoc" "member" "memv" "memq" "assq" "assv" "assoc" "delete"
    "delete!" "delete-duplicates" "delete-duplicates!" "length+" "acons"
    "alist-copy" "alist-delete" "alist-delete!" "any" "every" "filter" "filter!"
    "fold" "fold-right" "partition" "partition!" "remove" "remove!" "find" "find-tail"
    "take" "take!" "drop" "drop!" "split-at" "split-at!" "last" "last-pair"
    "append!" "append-reverse" "append-reverse!" "concatenate" "concatenate!"
    "reverse!" "count" "iota" "zip" "unzip1" "unzip2" "unzip3" "unzip4" "unzip5")
  "R7RS-specific built-in functions.")

(defconst scheme-colors-constants
  '("#t" "#f" "#true" "#false" "#\\newline" "#\\space" "#\\tab" "#\\return"
    "#\\null" "#\\alarm" "#\\backspace" "#\\escape" "#\\vtab" "#\\page"
    "#\\linefeed" "#\\rubout" "#\\x" "#\\u" "#\\U" "...")
  "Scheme constants and special values.")

(defconst scheme-colors-types
  '("boolean" "char" "string" "symbol" "number" "pair" "list" "vector" "procedure"
    "port" "input-port" "output-port" "bytevector" "eof-object" "error-object"
    "record-type" "syntax" "environment" "promise" "parameter" "hashtable")
  "Scheme type names.")

;; Combined list of all defined names that should NOT be colored as operators
(defconst scheme-colors-defined-names
  (append scheme-colors-r5rs-keywords
          scheme-colors-r7rs-keywords
          scheme-colors-builtin-functions
          scheme-colors-r7rs-builtin-functions
          scheme-colors-constants
          scheme-colors-types)
  "All defined names that should not be colored as operators.")

(defconst scheme-colors-syntax-highlighting
  `(
    ;; Comments - #808080 (Gray)
    (";.*" . 'scheme-colors-comment)
    ("#;.*" . 'scheme-colors-comment)
    ("#![^\n]*" . 'scheme-colors-comment)  ; Shebang comments
    
    ;; Strings - #ffd700 (Gold)
    ("\"\\(?:[^\"\\]\\|\\\\.\\)*\"" . 'scheme-colors-string)
    
    ;; Character literals - #daa520 (Goldenrod)
    ("#\\\\[^()\\[\\]{}\",'`;#|\\s]*" . 'scheme-colors-char)
    ("#\\\\space\\|#\\\\newline\\|#\\\\tab\\|#\\\\return" . 'scheme-colors-char)
    
    ;; Numbers - #f4a460 (Sandy Brown)
    ("\\_<-?[0-9]+\\_>" . 'scheme-colors-number)
    ("\\_<-?[0-9]+\\.[0-9]*\\_>" . 'scheme-colors-number)
    ("\\_<-?[0-9]+/[0-9]+\\_>" . 'scheme-colors-number)
    ("\\_<[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?\\_>" . 'scheme-colors-number)
    ("\\_<[-+]?[0-9]+/[-+]?[0-9]+\\_>" . 'scheme-colors-number)
    ("\\_<[-+]?[0-9]*\\.?[0-9]+[-+][0-9]*\\.?[0-9]+i\\_>" . 'scheme-colors-number)
    
    ;; Hex, binary, octal numbers - #f4a460 (Sandy Brown)
    ("#x[0-9a-fA-F]+\\_>" . 'scheme-colors-number)
    ("#b[01]+\\_>" . 'scheme-colors-number)
    ("#o[0-7]+\\_>" . 'scheme-colors-number)
    ("#d[0-9]+\\_>" . 'scheme-colors-number)
    
    ;; Constants and booleans - #32cd32 (Lime Green)
    (,(regexp-opt scheme-colors-constants 'symbols) . 'scheme-colors-constant)
    
    ;; R5RS Keywords - #00bfff (Deep Sky Blue)
    (,(regexp-opt scheme-colors-r5rs-keywords 'symbols) . 'scheme-colors-keyword)
    
    ;; R7RS Keywords - #00bfff (Deep Sky Blue)
    (,(regexp-opt scheme-colors-r7rs-keywords 'symbols) . 'scheme-colors-keyword)
    
    ;; R5RS Built-in functions - #4169e1 (Royal Blue)
    (,(regexp-opt scheme-colors-builtin-functions 'symbols) . 'scheme-colors-builtin)
    
    ;; R7RS Built-in functions - #4169e1 (Royal Blue)
    (,(regexp-opt scheme-colors-r7rs-builtin-functions 'symbols) . 'scheme-colors-builtin)
    
    ;; Type names - #da70d6 (Orchid)
    (,(regexp-opt scheme-colors-types 'symbols) . 'scheme-colors-type)
    
    ;; S-expression operators (first element) - #ff8c00 (Dark Orange) (excludes defined names)
    (,(lambda (limit)
        (let ((pos (point)))
          (catch 'found
            (while (and (< pos limit)
                        (setq pos (re-search-forward "(\\([^() \t\n]+\\)" limit t)))
              (let ((op (match-string 1)))
                (unless (member op scheme-colors-defined-names)
                  (throw 'found t)))))))
     1 'scheme-colors-operator)
    
    ;; Function definitions - #00ffff (Cyan)
    ("(\\(define\\)\\s-+(\\(\\sw+\\)" (1 'scheme-colors-keyword) (2 'scheme-colors-function))
    ("(\\(define\\)\\s-+\\(\\sw+\\)" (1 'scheme-colors-keyword) (2 'scheme-colors-function))
    ("(\\(define-syntax\\)\\s-+\\(\\sw+\\)" (1 'scheme-colors-keyword) (2 'scheme-colors-function))
    ("(\\(define-values\\|define-record-type\\)\\s-+(\\(\\sw+\\)" 
     (1 'scheme-colors-keyword) (2 'scheme-colors-type))
    
    ;; Variable definitions - #87cefa (Light Sky Blue)
    ("(\\(define\\)\\s-+\\(\\sw+\\)\\s-+" (1 'scheme-colors-keyword) (2 'scheme-colors-variable))
    ("(\\(let\\|let\\*\\|letrec\\|letrec\\*\\)\\s-+(\\(\\sw+\\)" 
     (1 'scheme-colors-keyword) (2 'scheme-colors-variable))
    
    ;; Library declarations - #00bfff (Deep Sky Blue)
    ("(\\(define-library\\|import\\|export\\|include\\|include-ci\\)\\s-+" 
     1 'scheme-colors-keyword)
    
    ;; Pattern matching - #00bfff (Deep Sky Blue)
    ("(\\(match\\|match-lambda\\|match-let\\|match-let\\*\\)\\s-+" 
     1 'scheme-colors-keyword)
    
    ;; Error handling - #ff0000 (Red)
    ("(\\(guard\\|with-exception-handler\\|raise\\|raise-continuable\\)\\s-+" 
     1 'scheme-colors-error)
    
    ;; Parameters - #4169e1 (Royal Blue)
    ("(\\(make-parameter\\|parameterize\\)\\s-+" 1 'scheme-colors-builtin)
    
    ;; Quoting forms - #00bfff (Deep Sky Blue)
    ("['`]" . 'scheme-colors-keyword)
    (",@?" . 'scheme-colors-keyword)
    
    ;; Vector literals - #4169e1 (Royal Blue)
    ("#(" . 'scheme-colors-builtin)
    
    ;; Bytevector literals - #4169e1 (Royal Blue)
    ("#u8(" . 'scheme-colors-builtin)
    
    ;; Syntax for various number types - #f4a460 (Sandy Brown)
    ("#e\\|#i\\|#b\\|#o\\|#x\\|#d" . 'scheme-colors-number)
    
    ;; R7RS cond-expand features - #00bfff (Deep Sky Blue)
    ("(\\(library\\|and\\|or\\|not\\)\\s-+" 1 'scheme-colors-keyword)
    )
  "Syntax highlighting rules for Scheme.")

(defun scheme-colors-add-keywords ()
  "Add Scheme colors to the current major mode."
  (interactive)
  (font-lock-add-keywords nil scheme-colors-syntax-highlighting))

(defun scheme-colors-remove-keywords ()
  "Remove Scheme colors from the current major mode."
  (interactive)
  (dolist (keywords scheme-colors-syntax-highlighting)
    (font-lock-remove-keywords nil (list keywords))))

;; Hook for scheme-mode
(defun scheme-colors-setup ()
  "Setup scheme colors for scheme-mode."
  (when (derived-mode-p 'scheme-mode)
    (scheme-colors-add-keywords)))

;; Hook for geiser-mode (if present)
(defun scheme-colors-geiser-setup ()
  "Setup scheme colors for geiser-mode."
  (when (bound-and-true-p geiser-mode)
    (scheme-colors-add-keywords)))

;; Add hooks
(add-hook 'scheme-mode-hook 'scheme-colors-setup)
(add-hook 'geiser-mode-hook 'scheme-colors-geiser-setup)

;; Interactive functions
(defun scheme-colors-enable ()
  "Enable Scheme colors in the current buffer."
  (interactive)
  (scheme-colors-add-keywords)
  (font-lock-flush))

(defun scheme-colors-disable ()
  "Disable Scheme colors in the current buffer."
  (interactive)
  (scheme-colors-remove-keywords)
  (font-lock-flush))

;; Provide the package
(provide 'lang-scheme-colors)

;;; lang-scheme-colors.el ends here
