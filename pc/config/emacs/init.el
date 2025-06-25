;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-

;; ====================
;; Early Performance Tweaks
;; ====================
(setq gc-cons-threshold (* 50 1000 1000))  ; 50MB GC threshold during startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000)))) ; Reset to 2MB after init

;; ====================
;; Package Management
;; ====================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;;(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)  ; Debugging

;; ====================
;; Core UI/UX Settings
;; ====================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(global-tab-line-mode -1)

;; icons
(use-package all-the-icons :if (display-graphic-p))
;; run M-x all-the-icons-install-fonts

;; Font and Theme
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 120)
(use-package doom-themes
  :config
  (load-theme 'doom-homage-black t)
  (doom-themes-visual-bell-config))  ; Flash mode-line on error

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-minor-modes nil))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; ====================
;; Editing Enhancements
;; ====================
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)  ; Highlight matching parentheses
(delete-selection-mode 1)  ; Override selected text on typing

;; Backup Files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; ====================
;; Keybindings & Leader Key
;; ====================
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for custom leader commands.")
(global-set-key (kbd "C-c m") my-leader-map)

;; Example bindings
(define-key my-leader-map (kbd "t") #'treemacs)  ; Leader + t for treemacs
(global-set-key (kbd "<f8>") #'treemacs)         ; F8 for treemacs
(global-set-key (kbd "<f9>") #'vterm-toggle)     ; F9 for vterm

;; ====================
;; Essential Packages
;; ====================
;; Ivy/Counsel/Swiper (Enhanced Search)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; corfu completions
;;(use-package corfu
;;  :init
;;  (global-corfu-mode))

;; company mode
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ; Show immediately

(use-package company-box
  :hook (company-mode . company-box-mode))

;; lsp mode
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (c-mode . lsp)
	 (go-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (sh-mode . lsp))
  :config
  (setq lsp-completion-provider :capf))
  ;;(setq lsp-prefer-flymake nil)) ; use flycheck instead

;; install the following in system
;; pyright, clang, yaml-language-server, bash-language-server

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

;; tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt) ;; or 'always
  (global-treesit-auto-mode))

;; Remap scheme-mode to scheme-ts-mode if Tree-sitter is available
(add-to-list 'major-mode-remap-alist
             '(scheme-mode . scheme-ts-mode))

;; Better font lock and optional folding
(setq treesit-font-lock-level 4)
(setq treesit-fold-enable t)

;; Lisp Development
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy)))

;; Smartparens configuration
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)  ; Enable globally
  (show-smartparens-global-mode 1)  ; Highlight matching pairs
  
  ;; Customize behavior
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil)
  
  ;; SLIME-specific integration
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; eldoc
(use-package eldoc-box
  (use-package eldoc-box
    :hook (slime-mode . eldoc-box-hover-mode)))

(add-hook 'slime-mode-hook #'eldoc-mode)

;; macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; ==================
;;  SYNTAX COLORING
;; ==================
(prism-mode -1)

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


;; ====================
;; Optional Add-ons
;; ====================
;; Terminal
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash"))  ; Customize shell if needed

(use-package vterm-toggle
  :after vterm
  :bind ("<f9>" . vterm-toggle))

;; File Explorer
(use-package treemacs
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

;; Git Integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Syntax Checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Snippets
(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Project Management
(use-package projectile
  :config (projectile-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; Helpful (Better Help Buffers)
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

;; ====================
;; Final Setup
;; ====================
;; Server mode (for emacsclient)
(require 'server)
(unless (server-running-p)
  (server-start))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
