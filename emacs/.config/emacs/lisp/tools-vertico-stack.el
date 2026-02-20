;;; tools-vertico-stack.el --- vertico and minibuffer -*- lexical-binding: t; -*-

;;; Commentary:
;; minibuffer enhencement

;;; Code:

;; ----------------------------------------------------------------------
;; Minibuffer (Vertico stack)
;; ----------------------------------------------------------------------

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-y"     . consult-yank-pop)))


(provide 'tools-vertico-stack)
;;; tools-vertico-stack.el ends here
