;;; core-ui-polish.el --- UI and visual polish -*- lexical-binding: t; -*-

;; ----------------------------
;; Theme
;; ----------------------------
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Load a theme, e.g., doom-one
;;   (load-theme 'doom-one t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree / treemacs themes if needed
;;   (doom-themes-neotree-config)
;;   ;; Corrects org-mode native fontification
;;   (doom-themes-org-config))

;; ----------------------------
;; Theme
;; ----------------------------
;; Load the custom Genesis Dark theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'genesis-dark t)

;; ----------------------------
;; Modeline
;; ----------------------------
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
;; ----------------------------
;; Icons
;; ----------------------------
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; ----------------------------
;; Minibuffer and completion polish
;; ----------------------------
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

;; ----------------------------
;; Line numbers and cursor line highlighting (already in core-ui)
;; ----------------------------
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; ----------------------------
;; Font settings
;; ----------------------------
(set-face-attribute 'default nil :font "Fira Code" :height 110)

(message "UI polish loaded successfully.")
(provide 'core-ui-polish)
