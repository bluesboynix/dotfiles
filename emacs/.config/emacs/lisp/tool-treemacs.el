;;; tool-treemacs.el --- Treemacs with colorized directories & file types -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure nil
  :defer t
  :bind (([f8] . treemacs)
         ("C-x t t" . treemacs)
         ("C-x t b" . treemacs-bookmark)
         ("C-x t f" . treemacs-find-file))
  :config
  ;; -----------------------------------------------------------------
  ;; 1. Disable all icon-related features
  ;; -----------------------------------------------------------------
  (setq treemacs-use-all-the-icons nil    ; no icons
        treemacs-use-nerd-icons nil       ; safety
        treemacs-no-png-images nil        ; allow PNG fallback (not used anyway)
        treemacs-icons-show-others nil
        treemacs-icons-show-children nil)

  ;; -----------------------------------------------------------------
  ;; 2. Define custom faces for directories and file types
  ;; -----------------------------------------------------------------
  (defface my/treemacs-dir-face
    '((t (:inherit font-lock-keyword-face :weight bold :foreground "DodgerBlue")))
    "Face for directories in treemacs."
    :group 'treemacs-faces)

  (defface my/treemacs-elisp-face
    '((t (:inherit font-lock-function-name-face :foreground "DarkOrange")))
    "Face for Emacs Lisp files."
    :group 'treemacs-faces)

  (defface my/treemacs-python-face
    '((t (:inherit font-lock-builtin-face :foreground "ForestGreen")))
    "Face for Python files."
    :group 'treemacs-faces)

  (defface my/treemacs-markdown-face
    '((t (:inherit font-lock-doc-face :foreground "MediumPurple")))
    "Face for Markdown files."
    :group 'treemacs-faces)

  ;; Add more as you like

  ;; -----------------------------------------------------------------
  ;; 3. Apply colorization via treemacs-custom-colorization
  ;; -----------------------------------------------------------------
  (setq treemacs-custom-colorization
        '(("\\/\\'"                     . my/treemacs-dir-face)      ; directories
          ("\\.el\\'"                   . my/treemacs-elisp-face)
          ("\\.py\\'"                   . my/treemacs-python-face)
          ("\\.md\\'"                   . my/treemacs-markdown-face)
          ("\\.org\\'"                  . my/treemacs-markdown-face) ; reuse
          ;; Add more extensions: ("\\.js\\'"   . my/treemacs-js-face)
          ))

  ;; -----------------------------------------------------------------
  ;; 4. Other reliable settings (no icons, just behavior)
  ;; -----------------------------------------------------------------
  (treemacs-filewatch-mode t)          ; auto-refresh on disk changes
  (treemacs-follow-mode t)             ; auto-highlight current file
  (treemacs-git-mode 'simple)          ; show git status with colors (no icons)
  (setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-indentation 2
        treemacs-show-hidden-files nil
        treemacs-project-follow-mode t
        treemacs-is-never-other-window nil))

;; Optional: dired integration (colorizes but no icons)
(with-eval-after-load 'treemacs
  (when (require 'treemacs-icons-dired nil t)
    ;; Disable icons in dired if the package loads it
    (setq treemacs-icons-dired-mode nil)))

(provide 'tool-treemacs)
;;; tool-treemacs.el ends here
