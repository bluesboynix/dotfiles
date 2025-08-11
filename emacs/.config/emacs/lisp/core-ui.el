;;; core-ui.el --- Core UI/UX Setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file configures the core user interface and user experience settings
;; for Emacs.  It includes:
;;
;; - Minimal GUI elements (no menu/scroll/tool bars)
;; - Font and theme setup (with fallbacks)
;; - Doom modeline customization
;; - Dashboard startup screen with icons and messages
;; - Icons support via `all-the-icons`
;; - Line and column number display
;;
;; All configurations are done using `use-package` for better structure and
;; modularity.  The goal is a clean, modern, informative Emacs UI while
;; keeping everything in a single, readable file.
;;
;;; Code:
;; -------------------------------
;; Global UI/UX Tweaks
;; -------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-tab-line-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      column-number-mode t
      line-number-mode t)

;; -------------------------------
;; Icons (UI Enhancer)
;; -------------------------------
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(when (and (display-graphic-p)
           (not (member "all-the-icons" (font-family-list))))
  (message "⚠️  Run M-x all-the-icons-install-fonts to install icon fonts."))

;; -------------------------------
;; Font Configuration
;; -------------------------------
(defvar my/default-font "Hack Nerd Font" "Primary font for Emacs.")
(defvar my/default-font-size 120 "Font size in 1/10 pt units.")

(unless (member my/default-font (font-family-list))
  (setq my/default-font "Monospace"))

(set-face-attribute 'default nil
                    :font my/default-font
                    :height my/default-font-size)

(set-face-attribute 'fixed-pitch nil
                    :font my/default-font
                    :height my/default-font-size)

(set-face-attribute 'variable-pitch nil
                    :font my/default-font
                    :height my/default-font-size)

;; -------------------------------
;; Theme Configuration
;; -------------------------------
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-homage-black t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Fallback theme if doom fails
(unless (custom-theme-enabled-p 'doom-homage-black)
  (load-theme 'wombat t))

;; -------------------------------
;; Doom Modeline Configuration
;; -------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-env-version nil)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-lsp t)
  (doom-modeline-time-icon nil)
  (doom-modeline-indent-info t)
  (doom-modeline-modal-icon nil))

;; -------------------------------
;; Dashboard Configuration
;; -------------------------------
(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (setq dashboard-startup-banner 'official
        dashboard-banner-logo-title "Welcome to Emacs, Commander!"
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda   . 5))
        dashboard-show-shortcuts nil
        dashboard-set-init-info t
        dashboard-page-separator "\n\n"
        dashboard-footer-messages
        '("Hacking time!"
          "Let the lisping begin."
          "Minimalism is power."))

  (dashboard-setup-startup-hook))

(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)
	 (css-mode . rainbow-mode)
	 (html-mode . rainbow-mode)))

(provide 'core-ui)
;;; core-ui.el ends here
