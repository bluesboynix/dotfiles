;;; tools-vertico-stack.el --- Vertico minibuffer stack -*- lexical-binding: t; -*-

;;; Commentary:
;; Minibuffer completion stack:
;; Vertico + Marginalia + Consult
;; Clean, minimal, project-aware.

;;; Code:

;; ------------------------------------------------------------
;; Vertico
;; ------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)) ;; cycle through candidates

;; ------------------------------------------------------------
;; Marginalia (annotations)
;; ------------------------------------------------------------

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; ------------------------------------------------------------
;; Consult (search/navigation)
;; ------------------------------------------------------------

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-y"     . consult-yank-pop)
         ("C-c p f" . consult-find)
         ("C-c p g" . consult-ripgrep)))

;; ------------------------------------------------------------
;; Better register preview
;; ------------------------------------------------------------

(setq register-preview-delay 0
      register-preview-function #'consult-register-format)

;; ------------------------------------------------------------
;; Use Consult for xref (clean integration)
;; ------------------------------------------------------------

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(provide 'tools-vertico-stack)
;;; tools-vertico-stack.el ends here
