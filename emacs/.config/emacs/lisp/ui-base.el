;;; ui-base.el --- Basic UI tweaks -*- lexical-binding: t; -*-

;; Scratch buffer settings
(setq initial-scratch-message "")        ;; Empty scratch buffer
(setq initial-major-mode 'lisp-interaction-mode) ;; Ensure Lisp evaluation mode

;; Disable startup screen and GUI elements
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show line numbers and highlight current line
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Show matching parentheses
(show-paren-mode 1)

;; Save cursor position and auto-revert buffers
(save-place-mode 1)
(global-auto-revert-mode 1)

;; Disable backup files like file.txt~
(setq make-backup-files nil)

;; Theme
;; Load the custom Genesis Dark theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'genesis-dark t)

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  ;; Optional: use the bar style
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-enable-word-count t
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-env-version t
        doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))

  (setq doom-modeline-bar-width 3)

;; Icons
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; Minibuffer and completion polish
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Line numbers and cursor line highlighting (already in core-ui)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Font settings
(set-face-attribute 'default nil :font "Fira Code" :height 120)

(message "UI loaded successfully.")

(provide 'ui-base)
;;; ui-base.el ends here
