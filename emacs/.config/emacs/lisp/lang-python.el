;;; lang-python.el --- Minimal Python setup (python-ts-mode + eglot) -*- lexical-binding: t; -*-

;; Step-by-step: just enable python-ts-mode and Eglot.
;; Requires Emacs 29+ with tree-sitter support.
;; Install LSP server: pip install basedpyright (or pyright)

;; -------------------------------------------------------------------
;; 1. Use python-ts-mode for .py files
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; -------------------------------------------------------------------
;; 2. Basic hook: just start eglot
;; -------------------------------------------------------------------
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; -------------------------------------------------------------------
;; 3. Eglot configuration: point to python LSP server
;; -------------------------------------------------------------------
(with-eval-after-load 'eglot
  ;; Prefer basedpyright, fallback to pyright
  (let ((server (cond
                 ((executable-find "basedpyright") '("basedpyright" "--stdio"))
                 ((executable-find "pyright") '("pyright" "--stdio"))
                 (t (warn "No Python LSP server found. Install: pip install basedpyright")))))
    (when server
      (add-to-list 'eglot-server-programs `(python-ts-mode . ,server)))))

(provide 'lang-python)
;;; lang-python.el ends here
