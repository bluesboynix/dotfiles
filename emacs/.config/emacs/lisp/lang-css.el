;;; lang-css.el --- CSS setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern CSS setup using tree-sitter + eglot + apheleia

;;; Code:

;; ============================================================
;; Tree-sitter Grammar Source
;; ============================================================

(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((css "https://github.com/tree-sitter/tree-sitter-css"))))

;; Auto-install if missing
(unless (treesit-language-available-p 'css)
  (ignore-errors
    (treesit-install-language-grammar 'css)))

;; ============================================================
;; CSS Tree-sitter Mode
;; ============================================================

(when (treesit-available-p)
  (add-to-list 'auto-mode-alist
               '("\\.css\\'" . css-ts-mode)))

;; ============================================================
;; Eglot (CSS LSP)
;; ============================================================

(use-package eglot
  :ensure nil
  :hook (css-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(css-ts-mode
                 . ("vscode-css-language-server" "--stdio"))))

;; ============================================================
;; Formatting (Prettier via Apheleia)
;; ============================================================

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  (setf (alist-get 'css-ts-mode apheleia-mode-alist)
        'prettier)

  (apheleia-global-mode +1))

;; ============================================================
;; Utilities
;; ============================================================

;; Highlight colors inside CSS
(use-package rainbow-mode
  :ensure t
  :hook (css-ts-mode . rainbow-mode))

(provide 'lang-css)
;;; lang-css.el ends here
