;;; core-git.el --- Git integration -*- lexical-binding: t; -*-

;; Magit: interactive Git interface
(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :bind (("C-x g" . magit-status)))  ;; quick shortcut

;; Optional: show inline Git changes
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(message "Git integration loaded successfully.")
(provide 'core-git)
