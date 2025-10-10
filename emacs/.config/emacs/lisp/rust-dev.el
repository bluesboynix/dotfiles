;;; rust-dev.el --- Opinionated Rust development setup -*- lexical-binding: t -*-

;; Author: You <you@example.com>
;; Version: 0.1
;; Keywords: rust, development, lsp, cargo
;; Package-Requires: ((emacs "26.1") (rust-mode "1.0") (lsp-mode "7.0") (cargo "0.9"))

;;; Commentary:

;; This file provides a “dev bundle” on top of rust-mode:
;;  * format-on-save
;;  * prettify symbols
;;  * LSP integration
;;  * Convenient keybindings for cargo / run / test / debug
;;  * Toggles and minor customizations  

;;; Code:

(require 'rust-mode)
(require 'lsp-mode)
(require 'cargo nil t)  ;; cargo.el is optional

(defgroup rust-dev nil
  "Extended Rust development settings."
  :group 'languages
  :prefix "rust-dev-")

(defcustom rust-dev-format-on-save t
  "If non-nil, run rust-format-buffer when saving a Rust file."
  :type 'boolean
  :group 'rust-dev)

(defcustom rust-dev-prettify-symbols t
  "If non-nil, enable prettify-symbols-mode in rust buffers."
  :type 'boolean
  :group 'rust-dev)

(defun rust-dev--maybe-format-buffer ()
  "Format buffer if `rust-dev-format-on-save` is non-nil and `rust-format-on-save` is nil."
  (when (and rust-dev-format-on-save
             (bound-and-true-p rust-mode)
             (fboundp 'rust-format-buffer))
    (rust-format-buffer)))

(defun rust-dev--setup-prettify ()
  "Enable prettify-symbols in rust-mode if desired."
  (when rust-dev-prettify-symbols
    (prettify-symbols-mode 1)))

(defun rust-dev--lsp-rust-setup ()
  "Setup LSP for Rust."
  (lsp))  ;; you may want to configure lsp-mode variables before this

(defun rust-dev--cargo-setup ()
  "Enable cargo minor mode if available."
  (when (featurep 'cargo)
    (cargo-minor-mode 1)))

(defun rust-dev-mode-setup ()
  "Setup for `rust-mode` in rust-dev."
  ;; Prefer spaces over tabs
  (setq indent-tabs-mode nil)
  ;; Prettify symbols if enabled
  (rust-dev--setup-prettify)
  ;; Cargo integration
  (rust-dev--cargo-setup)
  ;; LSP
  (rust-dev--lsp-rust-setup)
  ;; Format on save
  (add-hook 'before-save-hook #'rust-dev--maybe-format-buffer nil t)
  ;; Custom keybindings
  (let ((map rust-mode-map))
    ;; run tests
    (define-key map (kbd "C-c C-c t") #'rust-test)
    (define-key map (kbd "C-c C-c r") #'rust-run)
    (define-key map (kbd "C-c C-c c") #'rust-compile)
    (define-key map (kbd "C-c C-c l") #'rust-run-clippy)
    (define-key map (kbd "C-c C-d") #'rust-dbg-wrap-or-unwrap)
    ;; toggle mutability
    (define-key map (kbd "C-c C-m") #'rust-toggle-mutability)))

;;;###autoload
(define-minor-mode rust-dev-mode
  "Minor mode to augment `rust-mode` with dev conveniences."
  :lighter " RustDev"
  :group 'rust-dev
  (if rust-dev-mode
      (progn
        ;; Enable rust-mode (if not already)
        (unless (derived-mode-p 'rust-mode)
          (rust-mode))
        ;; Setup our things
        (rust-dev-mode-setup))
    ;; On disable: you might want to clean up hooks, but we'll leave it minimal
    (remove-hook 'before-save-hook #'rust-dev--maybe-format-buffer t)))

;;;###autoload
(add-hook 'rust-mode-hook #'rust-dev-mode)

(provide 'rust-dev)
;;; rust-dev.el ends here
