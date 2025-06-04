;; ~/.emacs.d/init.el

;; ====================
;; UI Cleanup and Startup Behavior
;; ====================

(require 'server)
(unless (server-running-p)
  (server-start))

;; Enable mouse support in terminal
(xterm-mouse-mode 1)

;; Basic editing UX
(electric-indent-mode 1)
(global-display-line-numbers-mode 1)

;; GUI elements
(menu-bar-mode -1)        ;; Enable/Disable menubar
(tool-bar-mode -1)        ;; Enable/Disable toolbar (uncommon in modern UIs)
(scroll-bar-mode -1)      ;; Enable/Disable scrollbars

;; Files and backups
(setq make-backup-files nil)    ;; Don't create backup~ files
(setq auto-save-default nil)    ;; Don't create #autosave# files

;; Dashboard setup
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq inhibit-startup-screen t)

;; Startup behavior
(setq initial-scratch-message nil)    ;; Empty *scratch* buffer

;; Font (optional)
(set-face-attribute 'default nil
  :font "Hack Nerd Font"
  :height 120)  ;; Adjust size (e.g., 110 = 11pt, 120 = 12pt)

;; Theme (optional)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-homage-black t))

;; -------------------------
;; 2. Custom File Setup
;; -------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; -------------------------
;; 2. Package System Setup
;; -------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------
;; 3. Package Declarations (use-package)
;; -------------------------
;; (sly, paredit, rainbow-delimiters, treemacs, treemacs icons, vterm etc.)

;; SLY
(use-package sly
  :ensure t
  :hook (lisp-mode . sly-mode)
  :config
  (setq inferior-lisp-program "sbcl"))

;; Paredit
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          sly-mrepl-mode) . paredit-mode))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          sly-mrepl-mode) . rainbow-delimiters-mode))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

;; Treemacs icons
(use-package treemacs-all-the-icons	; optional for pretty icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme 'all-the-icons))

(global-set-key (kbd "<f8>") #'treemacs)

;; Vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Optional: keybinding to toggle vterm popup
  (defun my-toggle-vterm ()
    "Toggle a persistent vterm buffer."
    (interactive)
    (let ((buf (get-buffer "*vterm*")))
      (if buf
          (if (eq (current-buffer) buf)
              (kill-buffer buf)
            (switch-to-buffer buf))
        (vterm "*vterm*"))))
  (global-set-key (kbd "<f9>") #'my-toggle-vterm))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25)            ;; Customize height
  (setq doom-modeline-bar-width 3)          ;; Thickness of left bar
  (setq doom-modeline-icon t)               ;; Show icons if possible
  (setq doom-modeline-major-mode-icon t)    ;; Show major mode icons
  (setq doom-modeline-minor-modes nil)      ;; Hide minor modes
  (setq doom-modeline-buffer-encoding nil)  ;; Hide encoding like UTF-8
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-env-version t))

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  ;; Ensure dashboard loads as the startup screen
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (setq dashboard-startup-banner 'official) ;; or 'logo or a path to a custom image
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))
;; run M-x: all-the-icons-ibuffer

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)) ;; only load if in GUI mode
;; Make sure to run M-x: all-the-icons-install-font

;; which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; ivy --> counsel, swiper, ivy-rich. all-the-ivy-rich
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order))))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy-rich all-the-icons)
  :init
  (all-the-icons-ivy-rich-mode 1))

;; Company mode
(use-package company-box
  :hook (company-mode . company-box-mode)
  :ensure t)

(provide 'init)
;;; init.el ends here

