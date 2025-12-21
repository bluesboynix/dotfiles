;;; tools-treemacs.el --- File Browser / Treemacs -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("<f8>" . my/treemacs-toggle))
  :config
  ;; ---------------------------------------------------------------------------
  ;; Integrations
  ;; ---------------------------------------------------------------------------
  (use-package treemacs-icons-dired
    :ensure t
    :hook (dired-mode . treemacs-icons-dired-enable-once))

  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t)

  (use-package treemacs-evil
    :after treemacs
    :ensure t)

  ;; ---------------------------------------------------------------------------
  ;; Treemacs behavior
  ;; ---------------------------------------------------------------------------
  (setq treemacs-is-never-other-window t
        treemacs-silent-refresh t
        treemacs-sorting 'alphabetic-asc
        treemacs-width 30
        treemacs-follow-after-init t)

  ;; ---------------------------------------------------------------------------
  ;; Smart toggle for Treemacs
  ;; ---------------------------------------------------------------------------
  (defun my/treemacs-open-at-current-directory ()
    "Open Treemacs and set its root to the current buffer's directory."
    (interactive)
    (let ((path (or (buffer-file-name) default-directory)))
      (if (treemacs-current-visibility)
          (treemacs-select-window)
        (treemacs)))
    ;; Change Treemacs root to match the current buffer
    (when-let ((path (or (buffer-file-name) default-directory)))
      (treemacs-add-and-display-current-project-exclusively)
      (treemacs-find-file)))

  (defun my/treemacs-toggle ()
    "Toggle Treemacs and reveal the current file directory as root."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs)
      (my/treemacs-open-at-current-directory))))

(provide 'tools-treemacs)
;;; tools-treemacs.el ends here
