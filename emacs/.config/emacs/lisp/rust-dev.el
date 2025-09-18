;;; rust-dev.el --- Rust development environment  -*- lexical-binding: t; -*-
;;; Commentary:
;; Rust setup:
;;   - rust-ts-mode (tree-sitter) or rust-mode fallback
;;   - lsp-mode + rust-analyzer for IDE features
;;   - corfu for completion popups
;;   - cargo for Cargo commands
;;   - toml-mode for editing Cargo.toml

;;; Code:

;; Keep rust-mode installed for utilities (rustfmt etc.)
(use-package rust-mode
  :defer t)

;; Prefer tree-sitter major mode if available (Emacs 29+)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (add-to-list 'major-mode-remap-alist
               '(rust-mode . rust-ts-mode)))

(use-package lsp-mode
  :hook ((rust-mode rust-ts-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  ;; Enable which backend to use for Rust
  (lsp-rust-analyzer-server-command '("rust-analyzer"))
  ;; Optional: format on save using rust-analyzer
  (lsp-enable-on-type-formatting nil)
  :config
  ;; You can tweak lsp-mode performance here if needed:
  ;; (setq lsp-idle-delay 0.5
  ;;       lsp-log-io nil)
  )

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package corfu
  :init
  (global-corfu-mode))

(use-package cargo
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

;; Common settings for both rust-mode and rust-ts-mode
(defun rust-dev/setup ()
  "Common Rust settings."
  (setq indent-tabs-mode nil
        rust-format-on-save t))

(add-hook 'rust-mode-hook    #'rust-dev/setup)
(add-hook 'rust-ts-mode-hook #'rust-dev/setup)

(provide 'rust-dev)
;;; rust-dev.el ends here
