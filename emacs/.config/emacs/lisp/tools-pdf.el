;;; pdf-tools.el --- PDF configuration for Emacs

(provide 'tools-pdf)

;; Ensure pdf-tools is installed
(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))

(require 'pdf-tools)

;; Initialize pdf-tools
(pdf-tools-install)

;; Always use pdf-view-mode for PDFs
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; 🔑 IMPORTANT: allow zoom persistence
(setq pdf-view-display-size nil)

;; Smooth zoom factor
(setq pdf-view-resize-factor 1.1)

;; Continuous PDF scrolling (no page reset)
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (setq-local pdf-view-continuous t)))

;; Pixel-precise scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Better mouse scrolling
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

;; Disable line numbers for PDF files
(defun core-pdf/disable-line-numbers ()
  "Disable line numbers for PDF viewing."
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'core-pdf/disable-line-numbers)

;; Key bindings
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "n")
              #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "p")
              #'pdf-view-previous-line-or-previous-page))

;; Sepia / paper-like colors for PDFs
(setq pdf-view-midnight-colors '("#3b2f2f" . "#efe6d8"))
;;           foreground (text)    background (paper)

(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(message "PDF tools configured")
