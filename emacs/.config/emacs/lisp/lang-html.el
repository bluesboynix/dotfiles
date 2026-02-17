;;; lang-html.el --- HTML setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern HTML setup using tree-sitter + eglot + apheleia

;;; Code:

;; ============================================================
;; Tree-sitter Grammar Source
;; ============================================================

(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((html "https://github.com/tree-sitter/tree-sitter-html"))))

;; Auto-install if missing
(unless (treesit-language-available-p 'html)
  (ignore-errors
    (treesit-install-language-grammar 'html)))

;; ============================================================
;; HTML Tree-sitter Mode
;; ============================================================

(when (treesit-available-p)
  (add-to-list 'auto-mode-alist
               '("\\.html?\\'" . html-ts-mode)))

;; ============================================================
;; Eglot (HTML LSP)
;; ============================================================

(use-package eglot
  :ensure nil
  :hook (html-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(html-ts-mode
                 . ("vscode-html-language-server" "--stdio"))))

;; ============================================================
;; Emmet (HTML expansion)
;; ============================================================

(use-package emmet-mode
  :ensure t
  :hook (html-ts-mode . emmet-mode))

;; ============================================================
;; Formatting (Prettier via Apheleia)
;; ============================================================

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  (setf (alist-get 'html-ts-mode apheleia-mode-alist)
        'prettier)

  (apheleia-global-mode +1))

(provide 'lang-html)
;;; lang-html.el ends here
