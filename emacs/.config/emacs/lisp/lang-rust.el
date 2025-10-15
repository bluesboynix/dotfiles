;;; lang-rust.el --- Rust IDE setup -*- lexical-binding: t; -*-

;; Rust major mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda ()
                       (setq tab-width 4)
                       (setq indent-tabs-mode nil))))

;; LSP via Eglot
(use-package eglot
  :ensure t
  :hook (rust-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.5))

;; Flymake for inline syntax checking
(use-package flymake
  :hook (rust-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

;; Format Rust code on save using rustfmt
(defun rust-format-before-save ()
  "Format Rust code before saving."
  (add-hook 'before-save-hook 'rust-format-buffer nil t))

(add-hook 'rust-mode-hook 'rust-format-before-save)

(message "Rust IDE module loaded successfully.")
(provide 'lang-rust)
