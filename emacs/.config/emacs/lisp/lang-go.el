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
;; Run Go
;; ============================================================

(defun lang-go-run ()
  "Run current Go file or project."
  (interactive)
  (save-buffer)
  (if (locate-dominating-file default-directory "go.mod")
      (compile "go run .")
    (compile (format "go run %s"
                     (shell-quote-argument buffer-file-name)))))

;; ============================================================
;; Go Local Setup
;; ============================================================

(defun lang-go-setup ()
  "Go local configuration."

  ;; Go uses tabs (official style)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (electric-indent-local-mode -1)
  (setq-local go-ts-mode-indent-offset 2)

  ;; Compilation default (overridden by dev-project if needed)
  (setq-local compile-command "go run .")

  ;; Clean trailing whitespace on save
  (add-hook 'before-save-hook
            (lambda ()
              (delete-trailing-whitespace))
            nil t)
  ;; Keybindings (buffer-local)
  (local-set-key (kbd "C-c C-r") #'lang-go-run))

(add-hook 'go-mode-hook #'lang-go-setup)
(add-hook 'go-ts-mode-hook #'lang-go-setup)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; ============================================================
;; File Associations
;; ============================================================

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; ============================================================
;; Provide
;; ============================================================

(provide 'lang-go)
;;; lang-go.el ends here
