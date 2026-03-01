;;; dev-diagnostics.el --- Diagnostics layer (Flymake) -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in diagnostics configuration.
;; Language-agnostic and UI-neutral.

;;; Code:

;; ============================================================
;; Flymake (Built-in)
;; ============================================================

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config

  ;; ----------------------------------------------------------
  ;; Performance
  ;; ----------------------------------------------------------

  (setq flymake-no-changes-timeout 0.8   ;; less jitter
        flymake-start-on-flymake-mode t)

  ;; ----------------------------------------------------------
  ;; UI Noise Reduction
  ;; ----------------------------------------------------------

  (setq flymake-show-diagnostics-at-end-of-line nil)

  ;; Disable legacy echo spam
  (setq flymake-suppress-zero-counters t))

;; ============================================================
;; Optional: Keybindings (Minimal & Clean)
;; ============================================================

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))

(provide 'dev-diagnostics)
;;; dev-diagnostics.el ends here
