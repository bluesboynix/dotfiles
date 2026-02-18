;;; utils-collection.el --- Minimal IDE utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Clean and minimal IDE utilities setup.

;;; Code:

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


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


(message "Core utilities loaded.")
(provide 'utils-collection)
;;; utils-collection.el ends here
