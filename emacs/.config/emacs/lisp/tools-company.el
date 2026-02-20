;;; tools-company.el --- comanpy mode -*- lexical-binding: t; -*-

;;; Commentary:
;; compnay mode for emacs

;;; Code:

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(provide 'tools-company)
;;; tools-company.el ends here
