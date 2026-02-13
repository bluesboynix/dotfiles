;;; lang-nim.el --- Modern Nim IDE setup (Emacs 30+) -*- lexical-binding: t; -*-
;;; Commentary:
;; Full-featured Nim development module
;; - nim-mode
;; - eglot (LSP)
;; - corfu + orderless + cape (modern completion)
;; - yasnippet (snippets)
;; - inline signature popup
;; - nimpretty formatting
;; - project helpers

;;; Code:

;; --------------------------------------------------
;; Core Nim mode
;; --------------------------------------------------
(use-package nim-mode
  :ensure t
  :mode ("\\.nim\\'" "\\.nims\\'" "\\.nimble\\'")
  :interpreter "nim"
  :hook (nim-mode . lang-nim-setup)
  :config
  (setq nim-indent-offset 2))

;; --------------------------------------------------
;; Completion UI (Corfu)
;; --------------------------------------------------
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preview-current t)
  (corfu-quit-no-match 'separator))

;; Better matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Extra completion sources
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Icons in completion (GUI only)
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; --------------------------------------------------
;; Snippets
;; --------------------------------------------------
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; Optional snippet collection
(use-package yasnippet-snippets
  :ensure t)

;; --------------------------------------------------
;; LSP via Eglot
;; --------------------------------------------------
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nim-mode . ("nimlangserver"))))

(defun lang-nim-eglot-setup ()
  (when (executable-find "nimlangserver")
    (eglot-ensure)))

;; --------------------------------------------------
;; Inline signature popup
;; --------------------------------------------------
(use-package eldoc-box
  :ensure t
  :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-only-multi-line t))

;; --------------------------------------------------
;; Common setup
;; --------------------------------------------------
(defun lang-nim-setup ()
  "Main Nim IDE configuration."
  (setq-local indent-tabs-mode nil
              tab-width 2)

  ;; Enable LSP
  (lang-nim-eglot-setup)

  ;; Flymake diagnostics
  (flymake-mode 1)

  ;; Eldoc inline help
  (eldoc-mode 1)

  ;; Completion
  (setq-local completion-at-point-functions
              (append completion-at-point-functions
                      (list #'cape-file
                            #'cape-dabbrev
                            #'cape-keyword))))

(add-hook 'nim-mode-hook #'lang-nim-setup)

;; --------------------------------------------------
;; Formatting
;; --------------------------------------------------
(defun lang-nim-format-buffer ()
  (interactive)
  (when (and buffer-file-name
             (executable-find "nimpretty"))
    (call-process "nimpretty" nil nil nil buffer-file-name)
    (revert-buffer t t t)
    (message "Formatted with nimpretty")))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'nim-mode)
              (lang-nim-format-buffer))))

;; --------------------------------------------------
;; Project helpers
;; --------------------------------------------------
(defun lang-nim-build ()
  (interactive)
  (compile "nimble build"))

(defun lang-nim-run ()
  (interactive)
  (compile "nimble run"))

(defun lang-nim-test ()
  (interactive)
  (compile "nimble test"))

;; --------------------------------------------------
;; Keybindings
;; --------------------------------------------------
(with-eval-after-load 'nim-mode
  (define-key nim-mode-map (kbd "C-c C-b") #'lang-nim-build)
  (define-key nim-mode-map (kbd "C-c C-r") #'lang-nim-run)
  (define-key nim-mode-map (kbd "C-c C-t") #'lang-nim-test)
  (define-key nim-mode-map (kbd "C-c C-f") #'lang-nim-format-buffer)
  (define-key nim-mode-map (kbd "C-c C-s") #'yas-insert-snippet))

;; --------------------------------------------------
(provide 'lang-nim)
;;; lang-nim.el ends here
