;;; lang-go.el --- Go configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Go language setup.
;; Language-specific only. No LSP/diagnostics/project logic here.

;;; Code:

;; ============================================================
;; Tree-sitter Mode (Emacs 29+)
;; ============================================================

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist
               '(go-mode . go-ts-mode)))

;; ============================================================
;; Go Local Setup
;; ============================================================

(defun lang-go-setup ()
  "Go local configuration."

  ;; Go uses tabs (official style)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)

  ;; Compilation default (overridden by dev-project if needed)
  (setq-local compile-command "go build ./...")

  ;; Clean trailing whitespace on save
  (add-hook 'before-save-hook
            (lambda ()
              (delete-trailing-whitespace))
            nil t))

(add-hook 'go-mode-hook #'lang-go-setup)
(add-hook 'go-ts-mode-hook #'lang-go-setup)

;; ============================================================
;; File Associations
;; ============================================================

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; ============================================================
;; Provide
;; ============================================================

(provide 'lang-go)
;;; lang-go.el ends here
