;;; tool-treemacs.el --- Treemacs configuration -*- lexical-binding: t; -*-

;; Dependencies
(ensure-package 'treemacs)
(ensure-package 'treemacs-nerd-icons)
(ensure-package 'treemacs-icons-dired)
(ensure-package 'nerd-icons)

(use-package treemacs
  :defer t
  :bind ([f8] . treemacs)
  :custom
  (treemacs-persist-file
   (expand-file-name ".cache/treemacs-persist"
                     user-emacs-directory))
  (treemacs-indentation 2)
  (treemacs-no-png-images t)
  (treemacs-show-hidden-files nil)
  (treemacs-project-follow-mode t)
  (treemacs-is-never-other-window nil)
  :config
  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1)
  (treemacs-git-mode 'simple))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "Nerd Icons"))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode 1))

(provide 'tool-treemacs)
;;; tool-treemacs.el ends here
