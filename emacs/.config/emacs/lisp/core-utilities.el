;;; core-utilities.el --- General IDE utilities -*- lexical-binding: t; -*-

;; --- Completion UI (Corfu) ---
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-preview-current nil))

;; --- Which-Key: shows available keybindings ---
(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'side-window))

;; --- Projectile: project navigation ---
(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  ;; Keybinding for projectile commands
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --- Completion sorting / filtering (Orderless) ---
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; --- Enhanced minibuffer UX (Vertico + Consult + Marginalia) ---
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; --- Git integration ---
(use-package magit
  :commands magit-status)

;; --- Snippet support ---
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; --- Built-in project management ---
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (magit-project-status "Magit"))))

;; --- LSP support via built-in Eglot ---
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (js-mode . eglot-ensure))
  :config
  (setq eglot-send-changes-idle-time 0.5))

;; --- Syntax checking (Flymake) ---
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

(message "Core utilities loaded successfully.")
(provide 'core-utilities)
