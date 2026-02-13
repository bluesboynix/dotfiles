;;; lang-nim.el --- Nim language support -*- lexical-binding: t; -*-
;;; Commentary:
;; Nim development setup using use-package.
;; Provides:
;; - nim-mode
;; - lsp-mode support (nimlangserver)
;; - format on save (nimpretty via lsp or nim-mode)
;; - flycheck
;; - company completion

;;; Code:

;; -----------------------------
;; nim-mode
;; -----------------------------
(use-package nim-mode
  :ensure t
  :mode ("\\.nim\\'" "\\.nims\\'")
  :interpreter "nim"
  :hook
  (nim-mode . lsp-deferred)
  :config
  ;; Basic indentation settings
  (setq nim-indent-offset 2))

;; -----------------------------
;; LSP Mode
;; Requires: nimble install nimlangserver
;; -----------------------------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (nim-mode . lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)) ;; use flycheck

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

;; -----------------------------
;; Company (completion)
;; -----------------------------
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; -----------------------------
;; Flycheck
;; -----------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; -----------------------------
;; Format on Save (nimpretty)
;; -----------------------------
(defun my/nim-format-buffer ()
  "Format Nim buffer using nimpretty."
  (when (eq major-mode 'nim-mode)
    (shell-command-to-string
     (format "nimpretty %s" (shell-quote-argument buffer-file-name)))
    (revert-buffer t t t)))

(add-hook 'before-save-hook #'my/nim-format-buffer)

(provide 'lang-nim)
;;; lang-nim.el ends here
