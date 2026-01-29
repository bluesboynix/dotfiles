;;; lang-rust.el --- Rust development environment -*- lexical-binding: t; -*-

;; ==================================================
;; Project-aware Cargo helpers
;; ==================================================
(require 'project)

(defun rust--project-root ()
  "Return current project root or `default-directory`."
  (if-let ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun rust-cargo-compile (cmd)
  "Run Cargo CMD via compile-mode from project root."
  (let ((default-directory (rust--project-root)))
    (compile (format "cargo %s" cmd))))

(defun rust-cargo-build ()  (interactive) (rust-cargo-compile "build"))
(defun rust-cargo-run ()    (interactive) (rust-cargo-compile "run"))
(defun rust-cargo-test ()   (interactive) (rust-cargo-compile "test"))
(defun rust-cargo-check ()  (interactive) (rust-cargo-compile "check"))
(defun rust-cargo-clippy () (interactive) (rust-cargo-compile "clippy"))

;; ==================================================
;; Cargo prefix keymap (REAL keymap, not just labels)
;; ==================================================
(defvar rust-cargo-map (make-sparse-keymap)
  "Keymap for Cargo commands.")

(define-prefix-command 'rust-cargo-prefix)
(fset 'rust-cargo-prefix rust-cargo-map)

(define-key rust-cargo-map (kbd "b") #'rust-cargo-build)
(define-key rust-cargo-map (kbd "r") #'rust-cargo-run)
(define-key rust-cargo-map (kbd "t") #'rust-cargo-test)
(define-key rust-cargo-map (kbd "c") #'rust-cargo-check)
(define-key rust-cargo-map (kbd "l") #'rust-cargo-clippy)

;; ==================================================
;; Rust Mode
;; ==================================================
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

;; ==================================================
;; LSP Mode (rust-analyzer)
;; ==================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((rust-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; Flymake diagnostics
  (setq lsp-diagnostics-provider :flymake)

  ;; Rust Analyzer settings
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-proc-macro-enable t)

  :config
  (setq lsp-idle-delay 0.6
        lsp-log-io nil))

;; ==================================================
;; Flymake
;; ==================================================
(use-package flymake
  :ensure nil
  :hook (rust-mode . flymake-mode))

;; ==================================================
;; Cargo minor mode (optional, still useful)
;; ==================================================
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; ==================================================
;; which-key (Cargo menu)
;; ==================================================
(use-package which-key
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "C-c c" "cargo"))

;; ==================================================
;; Bind Cargo prefix to BOTH Rust modes
;; ==================================================
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c c") #'rust-cargo-prefix))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c c") #'rust-cargo-prefix))

;; ==================================================
;; DAP Mode (LLDB)
;; ==================================================
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :config
  (dap-auto-configure-mode)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)

  (dap-register-debug-template
   "Rust::LLDB Run"
   (list :type "lldb"
         :request "launch"
         :name "Rust::Run"
         :gdbpath "lldb"
         :target nil
         :cwd nil)))

(with-eval-after-load 'dap-mode
  (define-key rust-mode-map (kbd "<f5>")  #'dap-debug)
  (define-key rust-mode-map (kbd "<f9>")  #'dap-breakpoint-toggle)
  (define-key rust-mode-map (kbd "<f10>") #'dap-next)
  (define-key rust-mode-map (kbd "<f11>") #'dap-step-in)
  (define-key rust-mode-map (kbd "<f12>") #'dap-continue))

;; ==================================================
;; Tree-sitter (Emacs 29+)
;; ==================================================
(use-package treesit
  :ensure nil
  :config
  (when (treesit-available-p)
    (setq treesit-language-source-alist
          '((rust "https://github.com/tree-sitter/tree-sitter-rust")))
    (unless (treesit-language-available-p 'rust)
      (treesit-install-language-grammar 'rust))
    (add-to-list 'major-mode-remap-alist
                 '(rust-mode . rust-ts-mode))))

(provide 'lang-rust)
;;; lang-rust.el ends here
