;;; lang-go.el --- Clean Go configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Go setup using go-ts-mode + eglot (Emacs 29+)

;;; Code:

;; =========================
;; Go Tree-sitter Mode
;; =========================

(if (treesit-available-p)
    (use-package go-ts-mode
      :ensure nil  ; Built-in
      :mode "\\.go\\'"
      :hook ((go-ts-mode . eglot-ensure)
             (go-ts-mode . my/go-mode-setup))
      :config
      ;; Tree-sitter specific settings
      (setq go-ts-mode-indent-offset 4))
  
  ;; Fallback to classic go-mode
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :hook ((go-mode . eglot-ensure)
           (go-mode . my/go-mode-setup))
    :config
    ;; gofmt/goimports as fallback formatter
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)))

;; =========================
;; Go Mode Setup
;; =========================

(defun my/go-mode-setup ()
  "Custom setup for Go buffers."
  ;; Go uses tabs
  (setq-local indent-tabs-mode t
              tab-width 4
              standard-indent 4)
  
  ;; Format via gopls on save (only if eglot-managed)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t)
  
  ;; Display line numbers
  (display-line-numbers-mode 1)
  
  ;; Show trailing whitespace
  (setq-local show-trailing-whitespace t)
  
  ;; Enable eldoc for documentation
  (eldoc-mode 1))

;; =========================
;; Eglot (LSP via gopls)
;; =========================

(use-package eglot
  :ensure t
  :defer t
  :hook ((go-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure))
  :config
  ;; Add server for both modes
  (add-to-list 'eglot-server-programs
               '((go-ts-mode go-mode) . ("gopls")))
  
  (setq eglot-workspace-configuration
        '((gopls
           . ((staticcheck . t)
              (completeUnimported . t)
              (usePlaceholders . t)
              (matcher . "Fuzzy")
              (analyses
               . ((shadow . t)
                  (unusedparams . t)
                  (unusedwrite . t)
                  (nilness . t)
                  (fieldalignment . t)
                  (simplifycompositelit . t)
                  (simplifyrange . t)
                  (simplifyslice . t)))
              (codelenses
               . ((gc_details . t)
                  (generate . t)
                  (regenerate_cgo . t)
                  (tidy . t)
                  (upgrade_dependency . t)
                  (vendor . t)))))))
  
  ;; Performance tuning
  (setq eglot-confirm-server-initiated-edits nil
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:documentHighlightProvider))
  
  ;; Auto-shutdown when last buffer closes
  (add-hook 'eglot-managed-mode-hook
            (lambda () (unless eglot--managed-mode
                    (eglot-shutdown (eglot-current-server) t)))))

;; =========================
;; Flymake for diagnostics
;; =========================

(use-package flymake
  :ensure nil  ; Built-in
  :hook (go-ts-mode . flymake-mode))

;; =========================
;; Go Utility Commands
;; =========================

(defun my/go-test-package ()
  "Run go test for current package."
  (interactive)
  (save-buffer)
  (compile "go test -v ./..."))

(defun my/go-test-function ()
  "Run the test for the current function."
  (interactive)
  (save-buffer)
  (let* ((func-name (which-function))
         (test-name (if (string-match "^Test" func-name)
                        func-name
                      (concat "Test" (capitalize func-name)))))
    (compile (format "go test -v -run '^%s$'" test-name))))

(defun my/go-benchmark-function ()
  "Run benchmark for the current function."
  (interactive)
  (save-buffer)
  (let* ((func-name (which-function))
         (bench-name (if (string-match "^Benchmark" func-name)
                         func-name
                       (concat "Benchmark" (capitalize func-name)))))
    (compile (format "go test -bench='^%s$' -benchmem" bench-name))))

(defun my/go-generate ()
  "Run go generate."
  (interactive)
  (compile "go generate ./..."))

(defun my/go-mod-tidy ()
  "Run go mod tidy."
  (interactive)
  (compile "go mod tidy"))

(defun my/go-mod-vendor ()
  "Run go mod vendor."
  (interactive)
  (compile "go mod vendor"))

(defun my/go-vet ()
  "Run go vet."
  (interactive)
  (compile "go vet ./..."))

(defun my/go-lint ()
  "Run golangci-lint (if available)."
  (interactive)
  (if (executable-find "golangci-lint")
      (compile "golangci-lint run --color=always")
    (message "golangci-lint not installed. Install with: go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest")))

(defun my/go-run ()
  "Run the current file."
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name)))
    (if (string-match "\\.go$" file)
        (compile (format "go run %s" (shell-quote-argument file)))
      (message "Not a Go file"))))

(defun my/go-build ()
  "Build the current package."
  (interactive)
  (compile "go build"))

;; =========================
;; Project detection
;; =========================

(defun my/go-project-root ()
  "Find Go project root (where go.mod is)."
  (or (locate-dominating-file default-directory "go.mod")
      default-directory))

;; =========================
;; Keybindings - FIXED VERSION
;; =========================

;; Apply keybindings to go-ts-mode
(with-eval-after-load 'go-ts-mode
  ;; Testing
  (define-key go-ts-mode-map (kbd "C-c t t") #'my/go-test-package)
  (define-key go-ts-mode-map (kbd "C-c t f") #'my/go-test-function)
  (define-key go-ts-mode-map (kbd "C-c t b") #'my/go-benchmark-function)
  
  ;; Generate/Tidy
  (define-key go-ts-mode-map (kbd "C-c g g") #'my/go-generate)
  (define-key go-ts-mode-map (kbd "C-c g t") #'my/go-mod-tidy)
  (define-key go-ts-mode-map (kbd "C-c g v") #'my/go-mod-vendor)
  (define-key go-ts-mode-map (kbd "C-c g V") #'my/go-vet)  ; Changed to C-c g V to avoid conflict
  
  ;; Lint/Run/Build
  (define-key go-ts-mode-map (kbd "C-c l l") #'my/go-lint)
  (define-key go-ts-mode-map (kbd "C-c C-r") #'my/go-run)
  (define-key go-ts-mode-map (kbd "C-c C-b") #'my/go-build)
  
  ;; Eglot shortcuts
  (define-key go-ts-mode-map (kbd "C-c r a") #'eglot-code-actions)
  (define-key go-ts-mode-map (kbd "C-c r f") #'eglot-format)
  (define-key go-ts-mode-map (kbd "C-c r i") #'eglot-find-implementation)
  (define-key go-ts-mode-map (kbd "C-c r o") #'eglot-code-action-organize-imports)
  (define-key go-ts-mode-map (kbd "C-c r r") #'eglot-rename)
  (define-key go-ts-mode-map (kbd "C-c r t") #'eglot-find-type-definition)
  (define-key go-ts-mode-map (kbd "C-c r R") #'eglot-reconnect))

;; Apply same keybindings to classic go-mode
(with-eval-after-load 'go-mode
  ;; Testing
  (define-key go-mode-map (kbd "C-c t t") #'my/go-test-package)
  (define-key go-mode-map (kbd "C-c t f") #'my/go-test-function)
  (define-key go-mode-map (kbd "C-c t b") #'my/go-benchmark-function)
  
  ;; Generate/Tidy
  (define-key go-mode-map (kbd "C-c g g") #'my/go-generate)
  (define-key go-mode-map (kbd "C-c g t") #'my/go-mod-tidy)
  (define-key go-mode-map (kbd "C-c g v") #'my/go-mod-vendor)
  (define-key go-mode-map (kbd "C-c g V") #'my/go-vet)  ; Changed to C-c g V
  
  ;; Lint/Run/Build
  (define-key go-mode-map (kbd "C-c l l") #'my/go-lint)
  (define-key go-mode-map (kbd "C-c C-r") #'my/go-run)
  (define-key go-mode-map (kbd "C-c C-b") #'my/go-build)
  
  ;; Eglot shortcuts
  (define-key go-mode-map (kbd "C-c r a") #'eglot-code-actions)
  (define-key go-mode-map (kbd "C-c r f") #'eglot-format)
  (define-key go-mode-map (kbd "C-c r i") #'eglot-find-implementation)
  (define-key go-mode-map (kbd "C-c r o") #'eglot-code-action-organize-imports)
  (define-key go-mode-map (kbd "C-c r r") #'eglot-rename)
  (define-key go-mode-map (kbd "C-c r t") #'eglot-find-type-definition)
  (define-key go-mode-map (kbd "C-c r R") #'eglot-reconnect))

;; =========================
;; LSP Helper Commands
;; =========================

(defun my/go-lsp-status ()
  "Show LSP status."
  (interactive)
  (if (eglot-managed-p)
      (message "Eglot active: %s" (eglot-current-server))
    (message "Eglot NOT active. Try M-x eglot")))

(defun my/go-restart-lsp ()
  "Restart gopls."
  (interactive)
  (eglot-reconnect))

;; =========================
;; Mode line indicator
;; =========================

;; Simple lighter to show when eglot is active
(defvar my/go-mode-line
  '(:eval (if (eglot-managed-p) " Go[LSP]" " Go"))
  "Mode line lighter for Go.")

;; Add to mode line
(with-eval-after-load 'go-ts-mode
  (setq go-ts-mode-mode-line-string my/go-mode-line))

;; =========================
;; Install missing tools (optional)
;; =========================

(defun my/go-install-tools ()
  "Install recommended Go tools."
  (interactive)
  (let ((tools '("golang.org/x/tools/cmd/goimports@latest"
                 "golang.org/x/tools/gopls@latest"
                 "github.com/golangci/golangci-lint/cmd/golangci-lint@latest"
                 "github.com/go-delve/delve/cmd/dlv@latest")))
    (dolist (tool tools)
      (message "Installing %s..." tool)
      (shell-command (format "go install %s" tool)))))

(provide 'lang-go)

;;; lang-go.el ends here