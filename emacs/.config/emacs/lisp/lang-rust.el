;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Rust configuration.
;; - Tree-sitter support (Emacs 29+)
;; - rustfmt on save (if available)
;; - Cargo + rustc keybindings
;; - No LSP, no project frameworks

;;; Code:

;; ============================================================
;; Tree-sitter Remap (Emacs 29+)
;; ============================================================

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist
               '(rust-mode . rust-ts-mode)))

;; ============================================================
;; Customization
;; ============================================================

(defcustom rust-format-path "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rust)

;; ============================================================
;; Formatting
;; ============================================================

(defun lang-rust-format ()
  "Format buffer using rustfmt."
  (when (and (executable-find rust-format-path)
             (fboundp 'rust-format-buffer))
    (rust-format-buffer)))

;; ============================================================
;; Cargo Commands
;; ============================================================

(defun lang-rust-cargo-build ()
  "Run cargo build."
  (interactive)
  (compile "cargo build"))

(defun lang-rust-cargo-run ()
  "Run cargo run."
  (interactive)
  (compile "cargo run"))

(defun lang-rust-cargo-test ()
  "Run cargo test."
  (interactive)
  (compile "cargo test"))

(defun lang-rust-cargo-check ()
  "Run cargo check."
  (interactive)
  (compile "cargo check"))

;; ============================================================
;; rustc (Single File Compile)
;; ============================================================

(defun lang-rust-rustc ()
  "Compile current file using rustc."
  (interactive)
  (when buffer-file-name
    (compile
     (format "rustc %s"
             (shell-quote-argument buffer-file-name)))))

; ============================================================
;; Clippy
;; ============================================================
(defun lang-rust-cargo-clippy ()
  "Run cargo clippy."
  (interactive)
  (compile "cargo clippy"))

;; ============================================================
;; Smart Run
;; ============================================================

(defun lang-rust-smart-run ()
  "Use cargo run if Cargo.toml exists, otherwise rustc."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (lang-rust-cargo-run)
    (lang-rust-rustc)))

;; ============================================================
;; Setup
;; ============================================================

(defun lang-rust-setup ()
  "Rust local configuration."

  ;; Indentation
  (setq-local rust-indent-offset 2)
  (setq-local indent-tabs-mode nil)

  ;; Format + cleanup on save
  (add-hook 'before-save-hook #'lang-rust-format nil t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  (local-set-key (kbd "C-c C-l") #'lang-rust-cargo-clippy)
  
  ;; Keybindings
  (local-set-key (kbd "C-c C-c") #'lang-rust-cargo-build)
  (local-set-key (kbd "C-c C-r") #'lang-rust-smart-run)
  (local-set-key (kbd "C-c C-t") #'lang-rust-cargo-test)
  (local-set-key (kbd "C-c C-k") #'lang-rust-cargo-check)
  (local-set-key (kbd "C-c C-f") #'lang-rust-rustc))

(add-hook 'rust-mode-hook #'lang-rust-setup)
(add-hook 'rust-ts-mode-hook #'lang-rust-setup)


;; Force rust-ts-mode to NOT enable its own flymake backend
(defun lang-rust-disable-native-flymake ()
  (setq-local flymake-diagnostic-functions
              (remove 'rust-ts-flymake
                      flymake-diagnostic-functions)))

(add-hook 'rust-ts-mode-hook #'lang-rust-disable-native-flymake)

;; ============================================================
;; File Association
;; ============================================================

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ============================================================
;; Provide
;; ============================================================

(provide 'lang-rust)
;;; lang-rust.el ends here
