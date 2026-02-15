;;; utils-collection.el --- General IDE utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Collection of utilities for completion, navigation, and IDE features.

;;; Code:

;; ----------------------------------------------------------------------
;; Core completion - Different backends for GUI vs Terminal
;; ----------------------------------------------------------------------

(if (display-graphic-p)
    ;; GUI mode - Use Corfu with all the bells and whistles
    (progn
      (use-package corfu
        :ensure t
        :init
        (global-corfu-mode)
        :custom
        (corfu-auto t)
        (corfu-auto-delay 0.1)
        (corfu-auto-prefix 1)
        (corfu-preview-current nil)
        (corfu-min-width 20)
        (corfu-max-width 100)
        (corfu-count 14)
        (corfu-scroll-margin 4)
        
        :bind (:map corfu-map
                    ("M-p" . corfu-previous)
                    ("M-n" . corfu-next)
                    ("<tab>" . corfu-complete)
                    ("S-<return>" . corfu-insert)
                    ("C-g" . corfu-quit))
        
        :config
        ;; Enable popup info for documentation
        (use-package corfu-popupinfo
          :ensure nil
          :hook (corfu-mode . corfu-popupinfo-mode)))
      
      (message "Corfu loaded for GUI mode"))
  
  ;; Terminal mode - Use Icomplete (simpler, more reliable)
  (progn
    ;; Icomplete is built-in
    (use-package icomplete
      :ensure nil
      :config
      (icomplete-mode 1)
      (setq icomplete-show-matches-on-no-input t
            icomplete-compute-delay 0.1
            icomplete-max-delay-chars 3))
    
    ;; Fido-mode is also built-in - just call it directly
    (fido-mode 1)
    
    ;; Optionally use fido-vertical-mode for better display
    (when (fboundp 'fido-vertical-mode)
      (fido-vertical-mode 1))
    
    (message "Icomplete + Fido loaded for terminal mode")))

;; ----------------------------------------------------------------------
;; Completion style (Orderless) - Works in both GUI and terminal
;; ----------------------------------------------------------------------
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; ----------------------------------------------------------------------
;; Minibuffer completion (Vertico + Consult + Marginalia) 
;; Vertico works in terminal too, just without the graphical popup
;; ----------------------------------------------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-<return>" . vertico-exit-input)
              ("M-RET" . vertico-exit-input)))

(use-package consult
  :ensure t
  :bind (;; Search
         ("C-s" . consult-line)
         ("C-M-s" . consult-line-multi)
         ("M-s l" . consult-line)
         ("M-s g" . consult-grep)
         ("M-s f" . consult-find)
         ("M-s r" . consult-ripgrep)
         
         ;; Buffers and files
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         
         ;; Navigation
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g f" . consult-flymake)
         
         ;; Other
         ("M-y" . consult-yank-pop)
         ("M-s i" . consult-info))
  
  :custom
  (consult-preview-key 'any)
  (consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; ----------------------------------------------------------------------
;; Keybinding discovery (Which-key)
;; ----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 1
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'side-window)
  (which-key-min-display-lines 5)
  (which-key-max-description-length 30)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-show-remaining-keys t)
  :config
  (which-key-mode)
  
  ;; Custom replacements
  (which-key-add-key-based-replacements
    "C-c p" "projectile"
    "C-c !" "flymake"
    "C-c v" "version-control"
    "C-c e" "eglot"))

;; ----------------------------------------------------------------------
;; Project navigation (Projectile)
;; ----------------------------------------------------------------------
(use-package projectile
  :ensure t
  :defer 1
  :custom
  (projectile-completion-system 'default)
  (projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ----------------------------------------------------------------------
;; Built-in project management
;; ----------------------------------------------------------------------
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (magit-project-status "Magit"))))

;; ----------------------------------------------------------------------
;; Snippets
;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  
  ;; Add custom snippets directory
  (let ((snippets-dir (expand-file-name "snippets" user-emacs-directory)))
    (when (file-directory-p snippets-dir)
      (add-to-list 'yas-snippet-dirs snippets-dir)))
  
  ;; Keybindings
  (global-set-key (kbd "C-c y") #'yas-insert-snippet)
  (global-set-key (kbd "C-c n") #'yas-new-snippet))

;; ----------------------------------------------------------------------
;; Recent files
;; ----------------------------------------------------------------------
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 50
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\\.gz$" "\\.log$" ".*-autoloads\\.el$")))

;; ----------------------------------------------------------------------
;; History saving
;; ----------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

;; ----------------------------------------------------------------------
;; Environment setup
;; ----------------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ----------------------------------------------------------------------
;; Code folding
;; ----------------------------------------------------------------------
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c h h" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h l" . hs-hide-level)
              ("C-c h a" . hs-hide-all)
              ("C-c h A" . hs-show-all)))

;; ----------------------------------------------------------------------
;; Region expansion
;; ----------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; ----------------------------------------------------------------------
;; LSP support (Eglot) - Commented out by default
;; ----------------------------------------------------------------------
;; (use-package eglot
;;   :hook ((python-mode c-mode c++-mode go-mode js-mode rust-mode) . eglot-ensure)
;;   :custom
;;   (eglot-send-changes-idle-time 0.5)
;;   (eglot-ignored-server-capabilities '(:documentHighlightProvider))
;;   :bind (:map eglot-mode-map
;;               ("C-c a" . eglot-code-actions)
;;               ("C-c r" . eglot-rename)
;;               ("C-c f" . eglot-format)
;;               ("C-c h" . eglot-help-at-point)))

;; ----------------------------------------------------------------------
;; Syntax checking (Flymake)
;; ----------------------------------------------------------------------
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t))

(message "Core utilities loaded successfully.")
(provide 'utils-collection)
;;; utils-collection.el ends here
