;;; dev-core.el --- Core development defaults -*- lexical-binding: t; -*-
;;; Commentary:
;; Foundation configuration for all programming modes.
;; No LSP, no completion framework, no UI extras.
;; Safe, minimal, and fast.

;;; Code:

;; ============================================================
;; Indentation Defaults
;; ============================================================

(setq-default indent-tabs-mode nil
              tab-width 2)

;; ============================================================
;; Basic Programming UX
;; ============================================================

(defun dev-core-prog-mode-defaults ()
  "Sane defaults for programming modes."

  ;; Code folding
  (hs-minor-mode 1)

  ;; Show trailing whitespace in code
  ;; (setq show-trailing-whitespace t)

  ;; Sentence ends with single space
  (setq-local sentence-end-double-space nil))

(add-hook 'prog-mode-hook #'dev-core-prog-mode-defaults)

;; ============================================================
;; Compilation Behavior
;; ============================================================

(setq compilation-scroll-output t)

;; ============================================================
;; Large File Safety
;; ============================================================

(setq large-file-warning-threshold (* 100 1024 1024)) ;; 100MB

;; ============================================================
;; Performance Tweaks (Dev Focused)
;; ============================================================

;; Faster process output (important for LSP later)
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; ============================================================
;; Provide
;; ============================================================

(provide 'dev-core)
;;; dev-core.el ends here
