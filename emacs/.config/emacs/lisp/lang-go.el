;;; lang-go.el --- Minimal Go setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Go configuration using tree-sitter + eglot.
;; Requires: gopls installed.

;;; Code:

;; --------------------------------------------------
;; Tree-sitter Go
;; --------------------------------------------------
(when (treesit-available-p)

  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

  (use-package go-ts-mode
    :ensure nil
    :mode "\\.go\\'"
    :hook (go-ts-mode . my/go-mode-setup)))

;; --------------------------------------------------
;; Common Go Setup
;; --------------------------------------------------
(defun my/go-mode-setup ()
  "Minimal Go defaults."
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t) ;; Go uses tabs
  (eglot-ensure))

;; --------------------------------------------------
;; Eglot
;; --------------------------------------------------
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))

;; --------------------------------------------------
;; Format on save (gofmt via eglot)
;; --------------------------------------------------
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'go-ts-mode)
              (eglot-format-buffer))))

(provide 'lang-go)
;;; lang-go.el ends here
