;;; tools-orderless.el --- Completion -*- lexical-binding: t; -*-

;;; Commentary:
;; completion

;;; Code:

;; ----------------------------------------------------------------------
;; Completion Style
;; ----------------------------------------------------------------------

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

(provide 'tools-orderless)
;;; tools-orderless.el ends here
