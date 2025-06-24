;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-

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
;;(package-initialize)

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
(global-tab-line-mode 1)

;; icons
(use-package all-the-icons :if (display-graphic-p))
;; run M-x all-the-icons-install-fonts

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
  (dashboard-setup-startup-hook))

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

;; corfu completions
;;(use-package corfu
;;  :init
;;  (global-corfu-mode))

;; company mode
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ; Show immediately

(use-package company-box
  :hook (company-mode . company-box-mode))

;; lsp mode
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (c-mode . lsp)
	 (go-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (sh-mode . lsp))
  :config
  (setq lsp-completion-provider :capf))
  ;;(setq lsp-prefer-flymake nil)) ; use flycheck instead

;; install the following in system
;; pyright, clang, yaml-language-server, bash-language-server
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save))
  :config
  (setq gofmt-command "goimports"))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

;; tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt) ;; or 'always
  (global-treesit-auto-mode))

;; Remap scheme-mode to scheme-ts-mode if Tree-sitter is available
(add-to-list 'major-mode-remap-alist
             '(scheme-mode . scheme-ts-mode))

;; Better font lock and optional folding
(setq treesit-font-lock-level 4)
(setq treesit-fold-enable t)

;; Lisp Development
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy)))

;; Smartparens configuration
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)  ; Enable globally
  (show-smartparens-global-mode 1)  ; Highlight matching pairs
  
  ;; Customize behavior
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil)
  
  ;; SLIME-specific integration
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight quoted
(use-package highlight-quoted
  :hook (emacs-lisp-mode lisp-mode))

;; macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; ====================
;; Optional Add-ons
;; ====================
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
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

;; Git Integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Syntax Checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Snippets
(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Project Management
(use-package projectile
  :config (projectile-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

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
