;;; tools-orderless.el --- Completion style setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Orderless completion configuration.
;; Optimized for Vertico + Consult + Project + Eglot.

;;; Code:

(use-package orderless
  :ensure nil
  :init

  ;; ----------------------------------------------------------
  ;; Completion Styles
  ;; ----------------------------------------------------------

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion)))   ;; keep file paths intuitive
          (symbol (styles . (orderless)))          ;; better M-x, xref, etc.
          (project-file (styles . (orderless))))))

(provide 'tools-orderless)
;;; tools-orderless.el ends here
