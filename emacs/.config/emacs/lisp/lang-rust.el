;;; lang-rust.el --- Simple Rust setup for Emacs 30 -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Rust mode (Tree-sitter)
;; --------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; --------------------------------------------------
;; Eglot + rust-analyzer
;; --------------------------------------------------

(with-eval-after-load 'eglot
  ;; rust-analyzer
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer")))

  ;; Optional rust-analyzer settings
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer
                  (:cargo
                   (:allFeatures t))
                  :check
                  (:command "clippy"))))

;; --------------------------------------------------
;; start eglot
;; --------------------------------------------------

(defun my-rust-start-eglot ()
  "Start Eglot only inside Cargo projects."
  (when (and (executable-find "rust-analyzer")
             (locate-dominating-file default-directory "Cargo.toml"))
    (eglot-ensure)))

(add-hook 'rust-ts-mode-hook #'my-rust-start-eglot)

;; --------------------------------------------------
;; Simple Cargo commands
;; --------------------------------------------------

(defun my-rust-cargo-run ()
  "Run cargo run."
  (interactive)
  (compile "cargo run"))

(defun my-rust-cargo-build ()
  "Run cargo build."
  (interactive)
  (compile "cargo build"))

(defun my-rust-cargo-test ()
  "Run cargo test."
  (interactive)
  (compile "cargo test"))

(defun my-rust-run-file ()
  "Compile and run current Rust file using rustc."
  (interactive)

  (unless buffer-file-name
    (error "Buffer is not visiting a file"))

  (save-buffer)

  (let* ((file buffer-file-name)
         (output (file-name-sans-extension file))
         (cmd (format "rustc %s -o %s && %s"
                      (shell-quote-argument file)
                      (shell-quote-argument output)
                      (shell-quote-argument output))))
    (compile cmd)))


;; --------------------------------------------------
;; Keybindings
;; --------------------------------------------------

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-r") #'my-rust-cargo-run)
  (define-key rust-ts-mode-map (kbd "C-c C-b") #'my-rust-cargo-build)
  (define-key rust-ts-mode-map (kbd "C-c C-t") #'my-rust-cargo-test))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-c")
              #'my-rust-run-file))


(provide 'lang-rust)
;;; lang-rust.el ends here
