;;; tools-pdf.el --- PDF configuration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; PDF viewing with pdf-tools, including annotations, outlines, and comfortable reading.

;;; Code:

;; ----------------------------------------------------------------------
;; PDF Tools Core
;; ----------------------------------------------------------------------
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  ;; Zoom settings
  (pdf-view-display-size nil)  ; Allow zoom persistence
  (pdf-view-resize-factor 1.1) ; Smooth zoom factor
  
  ;; Colors for midnight mode (sepia/paper-like)
  (pdf-view-midnight-colors '("#3b2f2f" . "#efe6d8"))  ; (text . background)
  
  ;; History
  (pdf-history-auto-save t)
  (pdf-history-auto-save-interval 30)
  
  ;; Annotation defaults
  (pdf-annot-default-annotation-properties
   '((?H . ((type . highlight) (color . "#ffff00")))   ; Yellow highlight
     (?U . ((type . underline) (color . "#00ff00")))   ; Green underline
     (?N . ((type . note) (color . "#ff8800")))))      ; Orange note
  (pdf-annot-activate-created-annotations t)
  
  :config
  ;; Initialize pdf-tools
  (pdf-tools-install :no-query)  ; :no-query prevents confirmation prompt
  
  ;; Pixel-precise scrolling (Emacs 29+)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  
  ;; Mouse scrolling settings
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  
  ;; Custom PDF view mode setup
  (defun my/pdf-view-mode-hook ()
    "Custom hook for pdf-view-mode."
    ;; Disable line numbers
    (display-line-numbers-mode -1)
    ;; Auto-revert when file changes
    (auto-revert-mode 1)
    ;; Fit to width by default
    (pdf-view-fit-width-to-window)
    ;; Midnight mode for comfortable reading
    (pdf-view-midnight-minor-mode 1)
    ;; Save/restore position
    (when (fboundp 'pdf-view-restore-mode)
      (pdf-view-restore-mode 1))
    ;; Continuous scrolling
    (setq-local pdf-view-continuous t))
  
  (add-hook 'pdf-view-mode-hook #'my/pdf-view-mode-hook)
  
  ;; Imenu support
  (defun my/pdf-imenu-setup ()
    "Setup imenu for PDF outlines."
    (when (derived-mode-p 'pdf-view-mode)
      (setq imenu-create-index-function
            (lambda ()
              (when (and (fboundp 'pdf-outline-outlines)
                         (pdf-outline-outlines))
                (mapcar (lambda (outline)
                          (cons (pdf-outline-title outline)
                                (pdf-outline-page outline)))
                        (pdf-outline-outlines)))))))
  
  (add-hook 'pdf-view-mode-hook #'my/pdf-imenu-setup)

  ;; Key bindings for pdf-view-mode
  (define-key pdf-view-mode-map (kbd "n") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "p") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "N") #'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "P") #'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-command)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-command)
  
  ;; Zoom
  (define-key pdf-view-mode-map (kbd "+") #'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "-") #'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "=") #'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "0") #'pdf-view-scale-reset)
  
  ;; Search and navigation
  (define-key pdf-view-mode-map (kbd "C-s") #'pdf-occur)
  (define-key pdf-view-mode-map (kbd "o") #'pdf-outline)
  (define-key pdf-view-mode-map (kbd "b") #'pdf-outline-backward-page)
  (define-key pdf-view-mode-map (kbd "'") #'pdf-view-mark-group-pages)
  (define-key pdf-view-mode-map (kbd "C-c s") #'pdf-occur-isearch-match)
  
  ;; Text selection
  (define-key pdf-view-mode-map (kbd "w") #'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "M-w") #'pdf-view-kill-ring-save)
  
  ;; Annotations
  (define-key pdf-view-mode-map (kbd "a h") #'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "a u") #'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "a n") #'pdf-annot-add-note-annotation)
  (define-key pdf-view-mode-map (kbd "a d") #'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "a l") #'pdf-annot-list-annotations)
  
  ;; Presentation
  (define-key pdf-view-mode-map (kbd "f") #'my/pdf-fullscreen)
  (define-key pdf-view-mode-map (kbd "s") #'my/pdf-slideshow-next)
  (define-key pdf-view-mode-map (kbd "S") #'my/pdf-slideshow-previous)

  ;; Better text selection feedback
  (setq pdf-view-kill-ring-save-show-message t)

  ;; Position saving
  (setq pdf-view-restore-filename
        (expand-file-name "pdf-view-restore.el" user-emacs-directory))

  ;; ----------------------------------------------------------------------
  ;; Presentation / Slideshow Functions
  ;; ----------------------------------------------------------------------
  (defun my/pdf-fullscreen ()
    "Toggle fullscreen for PDF presentation."
    (interactive)
    (toggle-frame-fullscreen)
    (pdf-view-fit-height-to-window)
    (message "Fullscreen mode toggled"))

  (defun my/pdf-slideshow-next ()
    "Go to next slide/page with fit-to-height."
    (interactive)
    (pdf-view-next-page 1)
    (pdf-view-fit-height-to-window))

  (defun my/pdf-slideshow-previous ()
    "Go to previous slide/page with fit-to-height."
    (interactive)
    (pdf-view-previous-page 1)
    (pdf-view-fit-height-to-window))

  (defun my/pdf-presentation-mode ()
    "Toggle presentation mode for PDF."
    (interactive)
    (if (eq major-mode 'pdf-view-mode)
        (progn
          (pdf-view-fit-height-to-window)
          (setq-local pdf-view-display-size 'fit-height)
          (message "Presentation mode enabled"))
      (message "Not in PDF view mode")))

  ;; ----------------------------------------------------------------------
  ;; Quick Commands
  ;; ----------------------------------------------------------------------
  (defun my/pdf-open (filename)
    "Open PDF FILENAME with pdf-tools."
    (interactive "fOpen PDF: ")
    (find-file filename))

  (defun my/pdf-annotate-screenshot ()
    "Take screenshot of current PDF page and annotate."
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (let ((page (pdf-view-current-page)))
        (pdf-view-page-image page)
        (message "Screenshot of page %d taken" page)))))

(message "PDF tools configured successfully.")
(provide 'tools-pdf)
;;; tools-pdf.el ends here
