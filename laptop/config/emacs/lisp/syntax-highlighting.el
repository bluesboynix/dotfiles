;; =====================
;; SYNTAX-HIGHLIGHTING
;; =====================

(defface font-lock-quoted-symbol-face
  '((t (:foreground "#e5c07b" :weight bold)))
  "Face for quoted symbols like 'foo or #'bar.")

(defface font-lock-lisp-operator-face
  '((t (:foreground "#c678dd" :weight bold)))
  "Face for operators, user-defined functions, etc.")

(defface font-lock-boolean-face
  '((t (:foreground "#98c379" :weight bold)))
  "Face for booleans t and nil.")

(defface font-lock-type-predicate-face
  '((t (:foreground "#61afef")))
  "Face for type predicates like symbolp, numberp, etc.")

(defface font-lock-builtin-lisp-face
  '((t (:foreground "#61afef" :weight bold)))
  "Face for built-in Common Lisp functions.")

(defface font-lock-number-face
  '((t (:foreground "#d19a66" :weight bold)))
  "Face for numeric literals.")

(defface font-lock-loop-variable-face
  '((t (:foreground "#d7ba7d" :slant italic)))
  "Face for loop variables like in dolist or dotimes.")

(defface font-lock-user-macro-face
  '((t (:foreground "#56b6c2" :underline t)))
  "Face for user-defined macros.")

(defface font-lock-bold-keyword-face
  '((t (:foreground "#c678dd" :weight bold :slant italic)))
  "Extra emphasis for special macros like loop, defmacro, case.")

;; highlighting function
(defun my-lisp-highlighting ()
  "Enhanced Lisp syntax highlighting."
  (font-lock-add-keywords
   nil
   `(
     ;; Macros and special forms (purple)
     ("\\<\\(defun\\|defmacro\\|lambda\\|let\\*?\\|if\\|cond\\|when\\|unless\\|quote\\|function\\|progn\\|and\\|or\\|case\\|list\\|loop\\|do\\|dotimes\\|dolist\\|for\\)\\>"
      . 'font-lock-keyword-face)

     ;; Function name after defun/defmacro (cyan)
     ("\\<\\(defun\\|defmacro\\)\\s +\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (2 'font-lock-function-name-face))

     ;; Arguments in defun/lambda (gold)
     ("(\\(?:defun\\|lambda\\)\\s +\\(?:\\sw\\|\\s_\\)+\\s +(\\([^)]*\\))"
      (1 'font-lock-variable-name-face))

     ;; Quoted symbols (yellow)
     ("\\(['`]\\|#'\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (2 'font-lock-quoted-symbol-face))

     ;; Quoted lists
     ("['`]\\(([^)]*)\\)"
      (1 'font-lock-quoted-symbol-face))

     ;; Strings (yellow)
     ("\"[^\"]*\"" . 'font-lock-string-face)

     ;; List operator (purple)
     ("\\<\\(list\\)\\>" . 'font-lock-keyword-face)

     ;; Function/operator names in head position (purple)
     ("(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 'font-lock-lisp-operator-face))

     ;; Booleans: t, nil (green)
     ("\\<\\(t\\|nil\\)\\>" . 'font-lock-boolean-face)

     ;; Type predicates (blue-ish)
     ("\\<\\(symbolp\\|numberp\\|stringp\\|atom\\|listp\\|consp\\|functionp\\|arrayp\\|characterp\\|hash-table-p\\|sequencep\\|vectorp\\|integerp\\|floatp\\|realp\\|rationalp\\|complexp\\)\\>"
      . 'font-lock-type-predicate-face)

     ;; Common Lisp built-in functions (bright blue)
("\\<\\(car\\|cdr\\|cons\\|list\\|append\\|reverse\\|nth\\|length\\|assoc\\|member\\|mapcar\\|mapc\\|mapcan\\|print\\|format\\|apply\\|funcall\\|equal\\|eq\\|eql\\|type-of\\|reduce\\|remove\\|find\\|position\\|every\\|some\\|notany\\|notevery\\|identity\\|complement\\)\\>"
 . 'font-lock-builtin-lisp-face)

     ;; Numbers (integers, floats)
     ("\\_<[-+]?[0-9]+\\(\\.[0-9]*\\)?\\_>" . 'font-lock-number-face)

     ;; Highlight macro name after defmacro
     ("\\<defmacro\\s +\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 'font-lock-user-macro-face))

     ;; Loop constructs (bold+italic purple)
     ("\\<\\(loop\\|dotimes\\|dolist\\|do\\|for\\)\\>" . 'font-lock-bold-keyword-face)

     ;; Loop variables (inside dolist/dotimes)
     ("(\\(dolist\\|dotimes\\)\\s +(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (2 'font-lock-loop-variable-face))
     )))

;; Apply to Lisp modes
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                slime-repl-mode-hook))
  (add-hook hook #'my-lisp-highlighting))


(custom-set-faces
 ;; Special forms / macros
 '(font-lock-keyword-face ((t (:foreground "#c678dd" :weight bold))))

 ;; Function names
 '(font-lock-function-name-face ((t (:foreground "#56b6c2" :weight bold))))

 ;; Function arguments
 '(font-lock-variable-name-face ((t (:foreground "#d7ba7d"))))

 ;; Quoted items + strings
 '(font-lock-quoted-symbol-face ((t (:foreground "#e5c07b" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#e5c07b"))))

 ;; Operators and head symbols
 '(font-lock-lisp-operator-face ((t (:foreground "#c678dd" :weight bold))))

 ;; Booleans t / nil
 '(font-lock-boolean-face ((t (:foreground "#98c379" :weight bold))))

 ;; Type predicates (symbolp, etc.)
 '(font-lock-type-predicate-face ((t (:foreground "#61afef"))))

 ;; custom-set-faces
 '(font-lock-builtin-lisp-face ((t (:foreground "#61afef" :weight bold))))

 ;; comment
 '(font-lock-comment-face ((t (:foreground "#5c6370" :slant italic))))

  ;; Numbers
 '(font-lock-number-face ((t (:foreground "#d19a66" :weight bold))))

 ;; Loop variables
 '(font-lock-loop-variable-face ((t (:foreground "#d7ba7d" :slant italic))))

 ;; User-defined macros
 '(font-lock-user-macro-face ((t (:foreground "#56b6c2" :underline t))))

 ;; Bold+italic special forms (loop, defmacro, etc.)
 '(font-lock-bold-keyword-face ((t (:foreground "#c678dd" :weight bold :slant italic))))
 )

(provide 'syntax-highlighting)
