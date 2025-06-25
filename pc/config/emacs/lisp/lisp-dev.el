;; Lisp Development
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy)))

;; Smartparens configuration
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)  ; Enable globally
  (show-smartparens-global-mode 1)  ; Highlight matching pairs
  
  ;; Customize behavior
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil)
  
  ;; SLIME-specific integration
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; eldoc
(use-package eldoc-box
  (use-package eldoc-box
    :hook (slime-mode . eldoc-box-hover-mode)))

(add-hook 'slime-mode-hook #'eldoc-mode)

;; macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(provide 'lisp-dev)
