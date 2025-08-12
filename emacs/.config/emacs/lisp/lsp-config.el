;;; lsp-config.el --- LSP and Tree-sitter configuration

;;; Commentary:
;; Sets up lsp-mode and tree-sitter for Python, Bash, and other languages.

;;; Code:

;; ---------------------
;; LSP Core Configuration
;; ---------------------
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (go-mode . lsp)
         (yaml-mode . lsp)
         (sh-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-completion-provider :capf
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable t))

;; ---------------------
;; Language-specific LSP Clients
;; ---------------------

;; Python (pyright)
(use-package lsp-pyright
  :if (executable-find "pyright")
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-deferred
  :config
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t))

;; Bash (bash-language-server)
;; No extra package needed beyond lsp-mode and the system binary

;; ---------------------
;; LSP UI Enhancements
;; ---------------------
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t))

;; ---------------------
;; Tree-sitter Integration
;; ---------------------
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Remap major modes to Tree-sitter versions (if available), excluding Scheme
(with-eval-after-load 'treesit
  (dolist (remap '((python-mode . python-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (go-mode . go-ts-mode)
                   (yaml-mode . yaml-ts-mode)
                   (sh-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap)))

;; Optional font lock and folding improvements
(setq treesit-font-lock-level 4)
(setq treesit-fold-enable t)

(provide 'lsp-config)

;;; lsp-config.el ends here
