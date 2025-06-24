;; -*- lexical-binding: t; -*-

;; ====================
;; Early Performance Tweaks
;; ====================
(setq gc-cons-threshold (* 50 1000 1000))  ; 50MB GC threshold during startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000)))) ; Reset to 2MB after init

;; ====================
;; Package Management
;; ====================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)  ; Debugging

;; ====================
;; Core UI/UX Settings
;; ====================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Font and Theme
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 120)
(use-package doom-themes
  :config
  (load-theme 'doom-homage-black t)
  (doom-themes-visual-bell-config))  ; Flash mode-line on error

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-minor-modes nil))

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook)

;; ====================
;; Editing Enhancements
;; ====================
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)  ; Highlight matching parentheses
(delete-selection-mode 1)  ; Override selected text on typing

;; Backup Files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; ====================
;; Keybindings & Leader Key
;; ====================
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for custom leader commands.")
(global-set-key (kbd "C-c m") my-leader-map)

;; Example bindings
(define-key my-leader-map (kbd "t") #'treemacs)  ; Leader + t for treemacs
(global-set-key (kbd "<f8>") #'treemacs)         ; F8 for treemacs
(global-set-key (kbd "<f9>") #'vterm-toggle)     ; F9 for vterm

;; ====================
;; Essential Packages
;; ====================
;; Ivy/Counsel/Swiper (Enhanced Search)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; Lisp Development
(use-package slime
  :init (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . paredit-mode)
  :diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Terminal
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash"))  ; Customize shell if needed

(use-package vterm-toggle
  :after vterm
  :bind ("<f9>" . vterm-toggle))

;; File Explorer
(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :config
  (setq treemacs-is-never-other-window t))

(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

;; ====================
;; Optional Add-ons
;; ====================
;; Git Integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Syntax Checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Snippets
(use-package yasnippet
  :config (yas-global-mode))

;; Project Management
(use-package projectile
  :config (projectile-mode 1))

;; Helpful (Better Help Buffers)
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

;; ====================
;; Final Setup
;; ====================
;; Server mode (for emacsclient)
(require 'server)
(unless (server-running-p)
  (server-start))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
