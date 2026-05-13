;;; tool-treemacs.el --- Treemacs configuration with nerd-icons -*- lexical-binding: t; -*-

;; nerd-icons
(ensure-package 'treemacs-nerd-icons)
(ensure-package 'nerd-icons-dired)

;; ONLY treemacs – no icon packages
(ensure-package 'treemacs)

(use-package treemacs
  :ensure nil                ; already installed via core-package
  :defer t
  :bind (([f8] . treemacs)
         ("C-x t t" . treemacs)
         ("C-x t b" . treemacs-bookmark)
         ("C-x t f" . treemacs-find-file))
  :config
  ;; -----------------------------------------------------------------
  ;; Use nerd-icons – much more reliable than all-the-icons
  ;; -----------------------------------------------------------------
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "Nerd Icons")

  ;; Optional: if you also want the dired integration
  (when (require 'treemacs-icons-dired nil t)
    (treemacs-icons-dired-mode))

  ;; Auto-refresh when files change on disk
  (treemacs-filewatch-mode t)

  ;; Auto-follow current file in tree
  (treemacs-follow-mode t)

  ;; Show git status (simple is stable)
  (treemacs-git-mode 'simple)

  ;; Session persistence
  (setq treemacs-persist-file
        (expand-file-name ".cache/treemacs-persist" user-emacs-directory))

  ;; Visual tweaks
  (setq treemacs-indentation 2
        treemacs-no-png-images t          ; keep this – we use fonts
        treemacs-show-hidden-files nil
        treemacs-project-follow-mode t
        treemacs-is-never-other-window nil))

;; Optional: dired integration (safe, no icon problems)
(with-eval-after-load 'treemacs
  (when (require 'treemacs-icons-dired nil t)
    (treemacs-icons-dired-mode)))

(provide 'tool-treemacs)
;;; tool-treemacs.el ends here
