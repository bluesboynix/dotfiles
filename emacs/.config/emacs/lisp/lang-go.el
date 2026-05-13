;;; lang-go.el --- Go configuration with explicit PATH setup -*- lexical-binding: t; -*-

;; Step-by-step: Go with gopls using Eglot.
;; Add the directory containing both go and gopls.

;; -------------------------------------------------------------------
;; 1. Add Go binary directories to Emacs PATH (critical!)
;; -------------------------------------------------------------------
;; Common Go installation paths
(let ((go-paths '("~/go/bin"
                  "/usr/local/go/bin"
                  "/usr/local/bin"))) ; fallback
  (dolist (path go-paths)
    (when (file-directory-p path)
      (add-to-list 'exec-path path)
      ;; Also update PATH environment variable for subprocesses
      (setenv "PATH" (concat path ":" (getenv "PATH"))))))

;; Optional: verify that 'go' and 'gopls' are now found
;; Uncomment for debugging:
;; (message "go executable: %s" (executable-find "go"))
;; (message "gopls executable: %s" (executable-find "gopls"))

;; -------------------------------------------------------------------
;; 2. Configure `project` package to find Go module roots
;; -------------------------------------------------------------------
(require 'project)

(defun project-find-go-module (dir)
  "Find Go module root by looking for go.mod in DIR or its parents."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return the root directory of a Go module project."
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; -------------------------------------------------------------------
;; 3. Ensure go-mode is loaded (if not, install it)
;; -------------------------------------------------------------------
(require 'go-mode nil t)  ; try to load if available
(unless (featurep 'go-mode)
  ;; If go-mode is missing, tell the user to install it
  (warn "go-mode not found. Install it via M-x package-install RET go-mode RET"))

;; -------------------------------------------------------------------
;; 4. Basic mode and hook
;; -------------------------------------------------------------------
(add-hook 'go-mode-hook #'my-go-setup)

(defun my-go-setup ()
  "Common setup for Go buffers."
  ;; Start Eglot (LSP client)
  (eglot-ensure)
  
  ;; Go-specific indentation
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode t)
  
  ;; Enable Flymake for diagnostics
  (flymake-mode +1)
  
  ;; Keybindings
  (my-go-keybindings))

;; -------------------------------------------------------------------
;; 5. Eglot configuration for gopls
;; -------------------------------------------------------------------
(with-eval-after-load 'eglot
  ;; Force Eglot to use gopls (find it from exec-path)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  
  ;; Configure gopls settings
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (completeUnimported . t)
                           (matcher . "CaseSensitive"))))))

;; -------------------------------------------------------------------
;; 6. Save hook for formatting
;; -------------------------------------------------------------------
(defun my-go-format-before-save ()
  "Add eglot-format-buffer to before-save-hook at priority -10."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(add-hook 'go-mode-hook #'my-go-format-before-save)

;; -------------------------------------------------------------------
;; 7. Keybindings
;; -------------------------------------------------------------------
(defun my-go-keybindings ()
  "Local keymap for Go mode."
  (local-set-key (kbd "M-.") #'xref-find-definitions)
  (local-set-key (kbd "M-,") #'xref-pop-marker-stack)
  (local-set-key (kbd "C-c C-d") #'eglot-find-declaration)
  (local-set-key (kbd "C-c C-r") #'eglot-rename)
  (local-set-key (kbd "C-c C-a") #'eglot-code-actions)
  (local-set-key (kbd "C-c C-c") (lambda () (interactive) (compile "go build -v")))
  (local-set-key (kbd "C-c C-t") (lambda () (interactive) (compile "go test -v"))))

(provide 'lang-go)
;;; lang-go.el ends here
