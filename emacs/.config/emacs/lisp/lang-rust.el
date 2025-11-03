;;; lang-rust.el --- Rust IDE setup -*- lexical-binding: t; -*-

;; Rust major mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda ()
                       (setq tab-width 4)
                       (setq indent-tabs-mode nil)
                       ;; Add our own format hook
                       (add-hook 'before-save-hook #'my-rust-format-buffer nil t))))

;; Define safe formatter (replacement for old rust-format-before-save)
(defun my-rust-format-buffer ()
  "Format Rust buffer before saving using rustfmt if available."
  (when (and (eq major-mode 'rust-mode)
             (executable-find "rustfmt"))
    (ignore-errors
      (rust-format-buffer))))

;; Eglot for LSP
(use-package eglot
  :ensure t
  :hook (rust-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.5))

;; Flymake for syntax checking
(use-package flymake
  :hook (rust-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

(message "Rust IDE module loaded successfully.")
(provide 'lang-rust)
;;; lang-rust.el ends here
