;;; tools-company.el --- Company completion setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion framework for in-buffer completion.
;; Uses CAPF (LSP, etc.) as the only backend.
;; Clean integration with Eglot.

;;; Code:

(use-package company
  :hook (prog-mode . global-company-mode)
  :config

  ;; ----------------------------------------------------------
  ;; Use only CAPF (Eglot feeds this)
  ;; ----------------------------------------------------------

  (setq company-backends '(company-capf))

  ;; ----------------------------------------------------------
  ;; Performance & Responsiveness
  ;; ----------------------------------------------------------

  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1      ;; 0.0 can be too aggressive
        company-selection-wrap-around t
        company-tooltip-align-annotations t)

  ;; ----------------------------------------------------------
  ;; Cleaner UX
  ;; ----------------------------------------------------------

  (setq company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

(provide 'tools-company)
;;; tools-company.el ends here
