;;; core-file-browser.el --- File Browser / Treemacs -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"     . treemacs-select-window) ;; Focus sidebar
        ("C-x t t" . treemacs)              ;; Toggle Treemacs
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file))
  :config
  ;; Optional: theme-aware icon support
  (use-package treemacs-icons-dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))

  ;; Optional: integrate with projectile
  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t)
  
  ;; Optional: integrate with lsp
  (use-package treemacs-evil
    :after treemacs
    :ensure t)

  ;; Automatically open Treemacs for the current project
  (setq treemacs-is-never-other-window t)
  (setq treemacs-silent-refresh t)
  (setq treemacs-sorting 'alphabetic-asc)
  (setq treemacs-width 30))

(provide 'core-file-browser)
;;; core-file-browser.el ends here

