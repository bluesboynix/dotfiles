;;; lang-python.el --- Python configuration (tree-sitter + LSP + JIT REPL) -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Basic setup
;; --------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; --------------------------------------------------
;; Virtual environment (.venv)
;; --------------------------------------------------
(defun my-python-activate-venv ()
  "Activate .venv in project root, if present.
Modifies `exec-path' and `process-environment' buffer-locally."
  (when-let ((root (locate-dominating-file default-directory ".venv"))
             (venv (expand-file-name ".venv" root))
             (bin  (expand-file-name "bin" venv)))
    ;; Prepend bin/ to exec-path
    (setq-local exec-path (cons bin exec-path))
    ;; Set or replace VIRTUAL_ENV in process-environment
    (setq-local process-environment
                (cons (format "VIRTUAL_ENV=%s" venv)
                      (delete (format "VIRTUAL_ENV=%s" (getenv "VIRTUAL_ENV"))
                              process-environment)))))

;; --------------------------------------------------
;; LSP server (eglot + basedpyright/pyright)
;; --------------------------------------------------
(defun my-python-lsp-server ()
  "Return command line for the best available Python LSP server."
  (cond
   ((executable-find "basedpyright-langserver")
    '("basedpyright-langserver" "--stdio"))
   ((executable-find "pyright-langserver")
    '("pyright-langserver" "--stdio"))
   (t nil)))

(defun my-python-start-eglot ()
  "Start eglot if a suitable LSP server is present."
  (when (and (buffer-file-name)
             (my-python-lsp-server))
    (eglot-ensure)))

;; Register the server for python-ts-mode (once)
(with-eval-after-load 'eglot
  (when-let ((server (my-python-lsp-server)))
    (add-to-list 'eglot-server-programs
                 `(python-ts-mode . ,server))))

;; --------------------------------------------------
;; REPL (JIT interpreter)
;; --------------------------------------------------
(defvar my-python-interpreter
  (or (executable-find "python")
      (executable-find "python3")
      "python3")
  "Python interpreter command (with absolute path if found).")

(setq python-shell-interpreter my-python-interpreter)

(defun my-python-shell ()
  "Open or switch to the Python REPL buffer."
  (interactive)
  (unless (python-shell-get-process)
    (run-python nil nil t))
  (pop-to-buffer (process-buffer (python-shell-get-process))))

(defun my-python-send-dwim ()
  "Send region if active, otherwise send current statement to the REPL."
  (interactive)
  (unless (python-shell-get-process)
    (run-python nil nil t))
  (if (use-region-p)
      (python-ts-send-region (region-beginning) (region-end))
    (python-ts-send-statement)))

;; --------------------------------------------------
;; Run file / tests in compilation buffer
;; --------------------------------------------------
(defun my-python-run-file ()
  "Run the current Python file in a compilation buffer."
  (interactive)
  (save-buffer)
  (compile (format "%s %s"
                   my-python-interpreter
                   (shell-quote-argument (buffer-file-name)))))

(defun my-python-run-pytest ()
  "Run pytest from project root (looks for pyproject.toml or pytest.ini)."
  (interactive)
  (if-let ((root (or (locate-dominating-file default-directory "pyproject.toml")
                     (locate-dominating-file default-directory "pytest.ini"))))
      (compile (format "cd %s && pytest" (shell-quote-argument root)))
    (user-error "Not a pytest project (no pyproject.toml or pytest.ini)")))

;; --------------------------------------------------
;; Keybindings (python-ts-mode-map)
;; --------------------------------------------------
(with-eval-after-load 'python-ts-mode
  (let ((map python-ts-mode-map))
    (define-key map (kbd "C-c C-c") #'my-python-send-dwim)   ; JIT send statement/region
    (define-key map (kbd "C-c C-r") #'my-python-run-file)    ; run whole file
    (define-key map (kbd "C-c C-t") #'my-python-run-pytest)  ; run pytest
    (define-key map (kbd "C-c C-z") #'my-python-shell)))     ; open REPL

;; --------------------------------------------------
;; Hooks
;; --------------------------------------------------
(add-hook 'python-ts-mode-hook #'my-python-activate-venv)
(add-hook 'python-ts-mode-hook #'my-python-start-eglot)

(provide 'lang-python)
;;; lang-python.el ends here
