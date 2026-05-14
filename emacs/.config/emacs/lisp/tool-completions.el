;;; tool-completions.el --- Modern completion setup -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Core completion variables
;; --------------------------------------------------
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles partial-completion))))

;; --------------------------------------------------
;; Marginalia
;; --------------------------------------------------
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; --------------------------------------------------
;; vertico
;; --------------------------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)                 ;; cycle through candidates
  (vertico-resize t)                ;; grow/shrink minibuffer
  (vertico-count 10))               ;; show up to 10 candidates

;; Dim the minibuffer's default face (optional)
(set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "MediumSpringGreen")
(set-face-attribute 'completions-common-part nil :foreground "white" :weight 'normal)
(set-face-attribute 'completions-first-difference nil :foreground "yellow" :weight 'bold)

;; ;; --------------------------------------------------
;; Corfu
;; --------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-count 5)          ;; Show at most 5 candidates
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))



;; --------------------------------------------------
;; CAPE
;; --------------------------------------------------
(use-package cape
  :ensure t
  :init
  ;; Add useful completion backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

;; --------------------------------------------------
;; Better minibuffer behavior
;; --------------------------------------------------
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(savehist-mode 1)

;; -------------------------------------------------------------------
;; Control minibuffer/completions window size
;; -------------------------------------------------------------------
(temp-buffer-resize-mode 1)
(setq temp-buffer-max-height 10)
(setq completions-max-height 10)
(setq resize-mini-windows 'grow-only)
(setq max-mini-window-height 0.3)   ; or 0.33

;; --------------------------------------------------
;; TAB completion behavior
;; --------------------------------------------------
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)

;; --------------------------------------------------
;; Optional: cleaner UI
;; --------------------------------------------------
(setq completions-detailed t)
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(provide 'tool-completions)
;;; tool-completions.el ends here
