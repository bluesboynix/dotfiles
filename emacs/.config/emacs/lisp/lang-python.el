;;; lang-python.el --- Python setup with correct LSP server -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(defun my-eglot-safe-ensure ()
  "Start Eglot if a Python LSP server is available."
  (when (buffer-file-name)
    (eglot-ensure)))

(add-hook 'python-ts-mode-hook #'my-eglot-safe-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-ts-mode .
                 ,(lambda (_interactive _project)
                    (cond
                     ((executable-find "basedpyright-langserver")
                      '("basedpyright-langserver" "--stdio"))
                     ((executable-find "pyright-langserver")
                      '("pyright-langserver" "--stdio"))
                     (t (user-error "No Python LSP server found. Install: pip install basedpyright or pyright")))))))

(provide 'lang-python)
