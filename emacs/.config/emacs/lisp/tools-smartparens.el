;;; tools-smartparens.el --- Structural editing (Smartparens) -*- lexical-binding: t; -*-
;;; Comentary:

;;; Code

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (lisp-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-mode)
         (scheme-mode . smartparens-mode))
  :config
  (require 'smartparens-config)

  ;; Global pairing highlight
  (show-smartparens-global-mode 1)

  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil))

(provide 'tools-smartparens)
;;; tools-smartparens.el ends here
