;;; core-pdf.el --- PDF configuration for Emacs

(provide 'core-pdf)

;; Ensure pdf-tools is installed
(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))

(require 'pdf-tools)

;; Initialize
(pdf-tools-install)

;; PDF viewing settings
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(setq pdf-view-resize-factor 1.1)

;; Disable line numbers for PDF files
(defun core-pdf/disable-line-numbers ()
  "Disable line numbers for PDF viewing."
  (when (derived-mode-p 'pdf-view-mode)
    (display-line-numbers-mode -1)))

(add-hook 'pdf-view-mode-hook 'core-pdf/disable-line-numbers)

;; Key bindings
(define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd "C-c C-t") 'pdf-outline)

(message "PDF tools configured")
