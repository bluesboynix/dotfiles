;;; core-pdf.el --- PDF viewing support -*- lexical-binding: t; -*-

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Activate pdf-tools
  (pdf-tools-install-noverify)

  ;; Better defaults
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-continuous t)

  ;; Smooth scrolling
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

  ;; Zoom shortcuts
  (define-key pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "-") 'pdf-view-shrink)

  ;; Dark mode (inverts colors)
  (define-key pdf-view-mode-map (kbd "d") 'pdf-view-midnight-mode)

  ;; Midnight mode colors (soft dark)
  (setq pdf-view-midnight-colors '("#e0e0e0" . "#000000")) ;; fg / bg
)

(message "PDF support loaded.")
(provide 'core-pdf)
;;; core-pdf.el ends here
