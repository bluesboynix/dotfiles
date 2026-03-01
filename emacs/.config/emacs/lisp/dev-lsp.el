;;; dev-lsp.el --- LSP configuration (Eglot) -*- lexical-binding: t; -*-
;;; Commentary:
;; Clean and minimal Eglot setup.
;; No completion framework integration.
;; No formatting enforcement.
;; No UI opinion.

;;; Code:

;; ============================================================
;; Eglot (Built-in LSP Client)
;; ============================================================

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook (prog-mode . dev-lsp-maybe-start)
  :config

  ;; ----------------------------------------------------------
  ;; Performance
  ;; ----------------------------------------------------------

  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-report-progress nil
        eglot-events-buffer-size 0)

  ;; ----------------------------------------------------------
  ;; Silence noisy capabilities
  ;; ----------------------------------------------------------

  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider))

  ;; ----------------------------------------------------------
  ;; Use built-in JSON for speed (Emacs 29+)
  ;; ----------------------------------------------------------

  (setq eglot-events-buffer-config '(:size 0 :format full)))

;; ============================================================
;; Safety: Only start if project exists
;; ============================================================

(defun dev-lsp-maybe-start ()
  "Start Eglot only inside a project."
  (when (and (project-current)
             (derived-mode-p 'prog-mode)
             (not (derived-mode-p 'scheme-mode)))
    (eglot-ensure)))

;; If you prefer project-only LSP, comment the main hook above
;; and use this instead:
;; (add-hook 'prog-mode-hook #'dev-lsp-maybe-start)

(provide 'dev-lsp)
;;; dev-lsp.el ends here
