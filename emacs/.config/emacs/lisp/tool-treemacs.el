;;; tool-treemacs.el --- Treemacs configuration -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure nil
  :defer t
  :bind
  (([f8] . treemacs)
   ("C-x t t" . treemacs)
   ("C-x t b" . treemacs-bookmark)
   ("C-x t f" . treemacs-find-file))
  :config
  ;; ------------------------------
  ;; Icon support with all-the-icons
  ;; ------------------------------
  (when (require 'all-the-icons nil t)
    ;; Tell treemacs to use all-the-icons
    (setq treemacs-use-all-the-icons t
          treemacs-icons-show-others t
          treemacs-icons-show-children t)
    ;; Optional: explicit theme (most reliable)
    (when (fboundp 'treemacs-load-theme)
      (treemacs-load-theme "all-the-icons")))
  
  ;; Auto-refresh when files change on disk
  (treemacs-filewatch-mode t)

  ;; Auto-follow current file in tree
  (treemacs-follow-mode t)

  ;; Show git status
  (treemacs-git-mode 'simple)

  ;; Session persistence
  (setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))

  ;; Visual tweaks
  (setq treemacs-indentation 2
        treemacs-no-png-images t
        treemacs-show-hidden-files nil
        treemacs-project-follow-mode t
        treemacs-is-never-other-window nil))

;; Optional dired integration (safe)
(with-eval-after-load 'treemacs
  (when (require 'treemacs-icons-dired nil t)
    (treemacs-icons-dired-mode)))

(provide 'tool-treemacs)
;;; tool-treemacs.el ends here
