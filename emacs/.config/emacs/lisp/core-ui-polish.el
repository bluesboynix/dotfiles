;;; core-ui-polish.el --- UI and visual polish -*- lexical-binding: t; -*-

;; ----------------------------
;; Theme
;; ----------------------------
(use-package doom-themes
  :ensure t
  :config
  ;; Load a theme, e.g., doom-one
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree / treemacs themes if needed
  (doom-themes-neotree-config)
  ;; Corrects org-mode native fontification
  (doom-themes-org-config))

;; ----------------------------
;; Modeline
;; ----------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-state-icon t))

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
