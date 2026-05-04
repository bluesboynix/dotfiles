;;; dev-completion.el --- Completion UI (Corfu) -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal completion UI using Corfu.
;; Works with Eglot (CAPF backend).
;; No language-specific logic.

;;; Code:

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2))

;; Make TAB complete (smart behavior)
(setq tab-always-indent 'complete)

(provide 'dev-completion)
;;; dev-completion.el ends here
