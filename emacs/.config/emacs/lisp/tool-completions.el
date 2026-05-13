;;; tool-completions.el --- Minibuffer + In-buffer completion -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------
;; Package installation (assuming ensure-package is defined elsewhere)
;; -------------------------------------------------------------------
(ensure-package 'vertico)
(ensure-package 'orderless)
(ensure-package 'marginalia)
(ensure-package 'consult)
(ensure-package 'embark)
(ensure-package 'embark-consult)
(ensure-package 'corfu)
(ensure-package 'cape)
(ensure-package 'corfu-marginalia)

;; ===================================================================
;; 1. MINIBUFFER COMPLETION (Vertico stack)
;; ===================================================================

;; Vertico: vertical display
(use-package vertico
  :ensure nil
  :init
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "TAB") #'minibuffer-complete)
  (define-key vertico-map (kbd "M-TAB") #'vertico-insert)
  (vertico-multiform-mode))   ; required for Embark grid

;; Orderless: fuzzy matching
(use-package orderless
  :ensure nil
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  :config
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-flexible)))

;; Marginalia: annotations
(use-package marginalia
  :ensure nil
  :init
  (marginalia-mode))

;; Consult: enhanced search
(use-package consult
  :ensure nil
  :bind (("C-c k" . consult-line)
         ("C-c b" . consult-buffer)
         ("C-c o" . consult-outline)))

;; Embark: action menu
(use-package embark
  :ensure nil
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

;; Embark-Consult integration
(use-package embark-consult
  :ensure nil
  :after (embark consult)
  :demand t)

;; ===================================================================
;; 2. IN-BUFFER COMPLETION (Corfu stack)
;; ===================================================================

;; Corfu: popup UI
(use-package corfu
  :ensure nil
  :init
  (global-corfu-mode)
  :config
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "C-g") #'corfu-quit)

  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-popupinfo-mode t
        corfu-separator ?\s
        corfu-count 10))

;; Cape: extra backends
(use-package cape
  :ensure nil
  :bind (("C-c p p" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p d" . cape-dict)
         ("C-c p l" . cape-line))
  :config
  ;; Globally add useful backends to `completion-at-point-functions'
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Corfu-Marginalia: show annotations inside popup
(use-package corfu-marginalia
  :ensure nil
  :after marginalia
  :config
  (corfu-marginalia-mode))

;; Ensure Orderless style is also used for in-buffer completion
;; (already set globally by orderless configuration)

(provide 'tool-completions)
;;; tool-completions.el ends here
