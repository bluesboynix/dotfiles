;; ====================
;; Optional Add-ons
;; ====================
;; Terminal
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash"))  ; Customize shell if needed

(use-package vterm-toggle
  :after vterm
  :bind ("<f9>" . vterm-toggle))

;; File Explorer
(use-package treemacs
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

;; Git Integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Syntax Checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Snippets
(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Project Management
(use-package projectile
  :config (projectile-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; Helpful (Better Help Buffers)
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

(provide 'extras)
