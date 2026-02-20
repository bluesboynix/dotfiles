;;; tools-treemacs.el --- File Browser / Treemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Treemacs file tree browser with projectile integration and smart toggling.

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("<f8>" . my/treemacs-toggle)
   ("C-c t t" . treemacs)
   ("C-c t f" . treemacs-find-file)
   ("C-c t p" . treemacs-find-project))
  :custom
  ;; Core settings
  (treemacs-is-never-other-window t)
  (treemacs-silent-refresh t)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-width 25)
  (treemacs-follow-after-init t)
  (treemacs-show-hidden-files nil)  ; Don't show dotfiles by default
  (treemacs-show-cursor nil)        ; Cleaner look
  (treemacs-git-mode 'deferred)     ; Git integration mode
  (treemacs-file-event-delay 5000)  ; File watch delay (ms)
  (treemacs-change-root-without-asking t)  ; Don't ask when changing root

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

  ;; Optional: Magit integration
  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure t)

  ;; Optional: Persp-mode integration
  ;; (use-package treemacs-persp
  ;;   :after (treemacs persp-mode)
  ;;   :ensure t
  ;;   :config
  ;;   (treemacs-persp-mode 1))

  ;; Optional: All-the-icons integration
  (use-package treemacs-all-the-icons
    :after treemacs
    :ensure t
    :config
    (treemacs-load-theme "all-the-icons"))

  ;; Optional packages for dired-subtree (removes warnings)
  (use-package dired-filter
    :ensure t
    :after dired)

  (use-package dired-subtree
    :ensure t
    :after (dired dired-filter dired-details)
    :bind (:map dired-mode-map
                ("TAB" . dired-subtree-toggle))
    :config
    ;; Disable the specific features that cause warnings
    (setq dired-subtree-use-filter-p nil)  ; Disable dired-filter integration
    (setq dired-subtree-use-dired-details nil))  ; Disable dired-details integration

  ;; ---------------------------------------------------------------------------
  ;; Faces customization
  ;; ---------------------------------------------------------------------------
  (defface my/treemacs-directory-face
    '((t (:inherit font-lock-function-name-face :weight bold)))
    "Face for treemacs directories.")

  (defface my/treemacs-file-face
    '((t (:inherit default)))
    "Face for treemacs files.")

  ;; Customize treemacs faces
  (set-face-attribute 'treemacs-directory-face nil
                      :foreground "#61afef"
                      :weight 'bold)
  (set-face-attribute 'treemacs-file-face nil
                      :foreground "#abb2bf")
  (set-face-attribute 'treemacs-git-modified-face nil
                      :foreground "#e5c07b")
  (set-face-attribute 'treemacs-git-untracked-face nil
                      :foreground "#7f848e")
  (set-face-attribute 'treemacs-git-added-face nil
                      :foreground "#98c379")
  (set-face-attribute 'treemacs-git-renamed-face nil
                      :foreground "#61afef")
  (set-face-attribute 'treemacs-git-ignored-face nil
                      :foreground "#3e4452")

  ;; ---------------------------------------------------------------------------
  ;; Line number disabling
  ;; ---------------------------------------------------------------------------
  (defun my/disable-line-numbers-in-treemacs ()
    "Disable line numbers in treemacs buffers."
    (display-line-numbers-mode -1))

  (add-hook 'treemacs-mode-hook #'my/disable-line-numbers-in-treemacs)

  ;; ---------------------------------------------------------------------------
  ;; Smart toggle functions
  ;; ---------------------------------------------------------------------------
  (defun my/treemacs-open-at-current-directory ()
    "Open Treemacs and set its root to the current buffer's directory."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs-select-window)
      (treemacs))

    ;; Change Treemacs root to match the current buffer
    (let ((path (or (buffer-file-name) default-directory)))
      (when path
        (condition-case nil
            (progn
              (treemacs-add-and-display-current-project-exclusively)
              (treemacs-find-file))
          (error
           (message "Could not set treemacs root to %s" path))))))

  (defun my/treemacs-toggle ()
    "Toggle Treemacs and reveal the current file directory as root."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs)
      (my/treemacs-open-at-current-directory)))

  (defun my/treemacs-goto-current-file ()
    "Jump to current file in treemacs."
    (interactive)
    (treemacs-find-file)
    (treemacs-select-window))

  (defun my/treemacs-goto-parent ()
    "Go to parent directory in treemacs."
    (interactive)
    (when (eq major-mode 'treemacs-mode)
      (treemacs-go-up-one-level)))

  (defun my/treemacs-create-file (filename)
    "Create new file FILENAME in current treemacs directory."
    (interactive "sNew file name: ")
    (when (eq major-mode 'treemacs-mode)
      (treemacs-create-file filename)))

  (defun my/treemacs-create-dir (dirname)
    "Create new directory DIRNAME in current treemacs directory."
    (interactive "sNew directory name: ")
    (when (eq major-mode 'treemacs-mode)
      (treemacs-create-dir dirname)))

  ;; ---------------------------------------------------------------------------
  ;; Workspace management
  ;; ---------------------------------------------------------------------------
  (defun my/treemacs-rename-workspace (new-name)
    "Rename current treemacs workspace to NEW-NAME."
    (interactive "sNew workspace name: ")
    (treemacs-rename-workspace new-name))

  (defun my/treemacs-switch-workspace (name)
    "Switch to treemacs workspace NAME."
    (interactive "sWorkspace name: ")
    (treemacs-switch-workspace name))

  ;; ---------------------------------------------------------------------------
  ;; Additional keybindings for treemacs mode
  ;; ---------------------------------------------------------------------------
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "g") #'treemacs-refresh)
    (define-key treemacs-mode-map (kbd "C") #'my/treemacs-create-file)
    (define-key treemacs-mode-map (kbd "D") #'my/treemacs-create-dir)
    (define-key treemacs-mode-map (kbd "R") #'treemacs-rename-file)
    (define-key treemacs-mode-map (kbd "DEL") #'treemacs-delete-file)
    (define-key treemacs-mode-map (kbd "<backspace>") #'my/treemacs-goto-parent)
    (define-key treemacs-mode-map (kbd "?") #'treemacs-help)
    (define-key treemacs-mode-map (kbd "c") #'treemacs-copy-file)
    (define-key treemacs-mode-map (kbd "p") #'treemacs-toggle-show-dotfiles)
    (define-key treemacs-mode-map (kbd "h") #'treemacs-toggle-fixed-width)
    (define-key treemacs-mode-map (kbd "f") #'treemacs-find-file))

  ;; ---------------------------------------------------------------------------
  ;; Project root detection improvements
  ;; ---------------------------------------------------------------------------
  (defun my/treemacs-find-project-root ()
    "Find project root for current buffer and focus treemacs there."
    (interactive)
    (when-let ((project-root (projectile-project-root)))
      (treemacs--find-project-for-path project-root)
      (treemacs-select-window)))

  ;; ---------------------------------------------------------------------------
  ;; Hydra for treemacs (optional)
  ;; ---------------------------------------------------------------------------
  ;; (use-package treemacs-hydra
  ;;   :ensure t
  ;;   :after treemacs
  ;;   :bind ("C-c t h" . treemacs-hydra/body))

  ;; ---------------------------------------------------------------------------
  ;; Final message
  ;; ---------------------------------------------------------------------------
  (message "Treemacs configured with smart toggle and integrations."))

(provide 'tools-treemacs)
;;; tools-treemacs.el ends here
