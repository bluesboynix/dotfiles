;;; tool-completions.el --- Modern completion setup -*- lexical-binding: t; -*-

;;; Minibuffer completion
(setq completion-category-defaults nil
      completion-category-overrides
      '((file (styles partial-completion)))
      enable-recursive-minibuffers t
      tab-always-indent 'complete
      completion-cycle-threshold 3
      completions-detailed t
      read-extended-command-predicate
      #'command-completion-default-include-p)

(savehist-mode 1)
(minibuffer-depth-indicate-mode 1)

;;; Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10))

;;; Marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; Corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.10)
  (corfu-auto-prefix 2)

  ;; popup appearance
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-count 8)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt))

;;; CAPE
(use-package cape
  :ensure t
  :after corfu
  :init
  ;; File completion everywhere.
  (add-to-list 'completion-at-point-functions #'cape-file)

  ;; Dabbrev only if nothing else (e.g. Eglot) succeeds.
  (add-hook
   'completion-at-point-functions
   (cape-capf-super
    #'cape-dabbrev
    #'cape-keyword)))

;;; Completion window sizing
(temp-buffer-resize-mode 1)

(setq temp-buffer-max-height 10
      completions-max-height 10
      resize-mini-windows 'grow-only
      max-mini-window-height 0.3)

;;; Faces
(set-face-attribute 'minibuffer-prompt nil
                    :foreground "MediumSpringGreen"
                    :weight 'bold)

(set-face-attribute 'completions-common-part nil
                    :foreground "white")

(set-face-attribute 'completions-first-difference nil
                    :foreground "yellow"
                    :weight 'bold)

(provide 'tool-completions)
;;; tool-completions.el ends here
