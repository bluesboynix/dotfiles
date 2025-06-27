;;; completion.el --- Modern minibuffer and inline completion

;;; Commentary:
;; Replaces Ivy/Counsel/Company with Vertico/Orderless/Corfu/Cape stack.

;;; Code:

;; -------------------
;; Vertico for Minibuffer
;; -------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)) ;; cycle around

;; Enable richer annotations using Marginalia
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Orderless for powerful matching (fuzzy, regex, etc.)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; Consult for advanced commands (replaces Counsel/Swiper)
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

;; -------------------
;; Corfu for Inline Completion (like Company)
;; -------------------
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                ;; Allows cycling through candidates
  (corfu-auto t)                 ;; Enable auto-completion
  (corfu-auto-delay 0.0)
  (corfu-min-width 40)
  (corfu-max-width 70)
  (corfu-quit-no-match t)
  (corfu-preselect-first t))

;; Optional: better in terminal
(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode 1))

;; -------------------
;; Cape for completion backends
;; -------------------
(use-package cape
  :init
  ;; Add useful defaults to `completion-at-point-functions'
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; Optionally:
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

;; Optional: kind icons
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; must match Corfu's background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'completion)

;;; completion.el ends here
