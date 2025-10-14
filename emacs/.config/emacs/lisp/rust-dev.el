;;; rust-dev.el --- Opinionated Rust development setup -*- lexical-binding: t -*-

;; Author: You <you@example.com>
;; Version: 0.3
;; Keywords: rust, development, eglot, cargo, flymake
;; Package-Requires: ((emacs "29.1") (rust-mode "1.0") (eglot "1.15") (smartparens "1.11") (cargo "0.9"))

;;; Commentary:
;;
;; A complete Rust development environment using rust-mode (no tree-sitter):
;;  * Eglot for LSP
;;  * Smartparens for structured editing
;;  * hs-minor-mode for code folding
;;  * Clippy Flymake integration
;;  * Cargo integration and pretty symbols
;;  * Format-on-save
;;  * Clean keybindings and hooks
;;
;; 100% tree-sitter-free and works out of the box with rust-analyzer.

;;; Code:

(require 'rust-mode)
(require 'eglot)
(require 'smartparens)
(require 'smartparens-rust)
(require 'cargo nil t)

(defgroup rust-dev nil
  "Extended Rust development settings using Eglot."
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
  "Format buffer if `rust-dev-format-on-save` is enabled."
  (when (and rust-dev-format-on-save
             (derived-mode-p 'rust-mode)
             (fboundp 'rust-format-buffer))
    (rust-format-buffer)))

(defun rust-dev--setup-prettify ()
  "Set up pretty symbols for Rust operators."
  (when rust-dev-prettify-symbols
    (setq prettify-symbols-alist
          '(("->" . ?→)
            ("=>" . ?⇒)
            ("<=" . ?≤)
            (">=" . ?≥)
            ("!=" . ?≠)
            ("==" . ?≡)))
    (prettify-symbols-mode 1)))

(defun rust-dev--eglot-setup ()
  "Start Eglot for Rust, if available."
  (when (and (executable-find "rust-analyzer")
             (fboundp 'eglot-ensure))
    (eglot-ensure)))

(defun rust-dev--cargo-setup ()
  "Enable cargo minor mode if available."
  (when (featurep 'cargo)
    (cargo-minor-mode 1)))

(defun rust-dev--clippy-setup ()
  "Setup clippy-flymake integration with Eglot."
  (when (require 'clippy-flymake nil t)
    (clippy-flymake-setup-backend)
    ;; Prevent Eglot from suppressing Flymake backends
    (add-to-list 'eglot-stay-out-of 'flymake)
    (defun rust-dev--clippy-enable ()
      "Manually activate Flymake in Eglot buffers."
      (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
      (flymake-mode 1))
    (add-hook 'eglot-managed-mode-hook #'rust-dev--clippy-enable)))

(defun rust-dev--smartparens-setup ()
  "Enable smartparens and related bindings."
  (smartparens-mode 1)
  (require 'smartparens-rust)
  (define-key rust-mode-map (kbd "C-<right>")   #'sp-forward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-<left>")    #'sp-forward-barf-sexp)
  (define-key rust-mode-map (kbd "C-M-<right>") #'sp-backward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-M-<left>")  #'sp-backward-barf-sexp))

(defun rust-dev-mode-setup ()
  "Setup for `rust-mode` in rust-dev."
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        fill-column 100)
  (column-number-mode)
  (display-line-numbers-mode)
  (hs-minor-mode 1)
  (rust-dev--setup-prettify)
  (rust-dev--smartparens-setup)
  (rust-dev--cargo-setup)
  (rust-dev--eglot-setup)
  (rust-dev--clippy-setup)
  (add-hook 'before-save-hook #'rust-dev--maybe-format-buffer nil t)
  ;; Custom keybindings
  (let ((map rust-mode-map))
    (define-key map (kbd "C-c a") #'eglot-code-actions)
    (define-key map (kbd "C-c >") #'hs-show-all)
    (define-key map (kbd "C-c <") #'hs-hide-all)
    (define-key map (kbd "C-c ;") #'hs-toggle-hiding)
    (define-key map (kbd "C-c '") #'hs-hide-level)
    (define-key map (kbd "C-c C-c t") #'rust-test)
    (define-key map (kbd "C-c C-c r") #'rust-run)
    (define-key map (kbd "C-c C-c c") #'rust-compile)
    (define-key map (kbd "C-c C-c l") #'rust-run-clippy)
    (define-key map (kbd "C-c C-d")   #'rust-dbg-wrap-or-unwrap)
    (define-key map (kbd "C-c C-m")   #'rust-toggle-mutability)))

;;;###autoload
(define-minor-mode rust-dev-mode
  "Minor mode to augment `rust-mode` with Eglot, Smartparens, and Clippy."
  :lighter " RustDev"
  :group 'rust-dev
  (if rust-dev-mode
      (rust-dev-mode-setup)
    (remove-hook 'before-save-hook #'rust-dev--maybe-format-buffer t)))

;;;###autoload
(add-hook 'rust-mode-hook
          (lambda ()
            (when (fboundp 'rust-colors-mode)
              (rust-colors-mode 1))
            (rust-dev-mode 1)))

(provide 'rust-dev)
;;; rust-dev.el ends here
