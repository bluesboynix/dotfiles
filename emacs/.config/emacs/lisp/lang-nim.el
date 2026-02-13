;;; lang-nim.el --- Nim Full IDE Mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Full-featured Nim IDE using:
;; - nim-mode
;; - eglot (nimlangserver)
;; - corfu + orderless + cape
;; - yasnippet
;; - Flymake diagnostics
;; - Inlay hints
;; - Code actions
;; - Auto format + organize imports on save
;; - Project build/run/test helpers

;;; Code:

;; --------------------------------------------------
;; Nim Major Mode (Robust Setup)
;; --------------------------------------------------

(use-package nim-mode
  :ensure t
  :defer t
  :init
  ;; Register file extensions early (important!)
  (add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
  (add-to-list 'auto-mode-alist '("\\.nims\\'" . nim-mode))
  (add-to-list 'auto-mode-alist '("\\.nimble\\'" . nim-mode))
  (add-to-list 'interpreter-mode-alist '("nim" . nim-mode))
  :config
  (setq nim-indent-offset 2))



;; --------------------------------------------------
;; Completion Stack
;; --------------------------------------------------

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package cape
  :ensure t)

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-icon-margin-formatter))

;; --------------------------------------------------
;; Snippets
;; --------------------------------------------------

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; --------------------------------------------------
;; Eglot + nimlangserver
;; --------------------------------------------------
(use-package eglot
  :ensure nil  ;; built-in in Emacs 29+
  :hook (nim-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(nim-mode . ("nimlangserver"))))

;; --------------------------------------------------
;; IDE Features
;; --------------------------------------------------

(defun lang-nim-organize-imports ()
  "Apply LSP code action to organize imports."
  (when (bound-and-true-p eglot-managed-mode)
    (eglot-code-actions nil nil "source.organizeImports")))

(defun lang-nim-format-buffer ()
  "Format via LSP."
  (when (bound-and-true-p eglot-managed-mode)
    (eglot-format-buffer)))

(defun lang-nim-before-save ()
  "Full IDE save actions."
  (lang-nim-organize-imports)
  (lang-nim-format-buffer))

;; --------------------------------------------------
;; Project Commands
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
;; UI Enhancements
;; --------------------------------------------------

(defun lang-nim-ui-setup ()
  ;; Inlay hints (Emacs 30+)
  (when (fboundp 'eglot-inlay-hints-mode)
    (eglot-inlay-hints-mode 1))

  ;; Eldoc in echo area
  (eldoc-mode 1)

  ;; Flymake
  (flymake-mode 1)

  ;; Pretty diagnostics
  (setq-local flymake-no-changes-timeout 0.5))

;; --------------------------------------------------
;; Main Setup
;; --------------------------------------------------

(defun lang-nim-setup ()
  "Enable full Nim IDE experience."

  ;; Indentation
  (setq-local indent-tabs-mode nil
              tab-width 2)

  ;; LSP
  (lang-nim-eglot-ensure)

  ;; Completion sources
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)

  ;; UI Enhancements
  (lang-nim-ui-setup)

  ;; Format + organize imports on save (buffer local)
  (add-hook 'before-save-hook
            #'lang-nim-before-save
            nil t)

  ;; IDE Keybindings
  (define-key nim-mode-map (kbd "C-c C-b") #'lang-nim-build)
  (define-key nim-mode-map (kbd "C-c C-r") #'lang-nim-run)
  (define-key nim-mode-map (kbd "C-c C-t") #'lang-nim-test)
  (define-key nim-mode-map (kbd "C-c C-f") #'lang-nim-format-buffer)
  (define-key nim-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key nim-mode-map (kbd "C-c C-d") #'eldoc)
  (define-key nim-mode-map (kbd "C-c C-s") #'yas-insert-snippet))

;; --------------------------------------------------

(provide 'lang-nim)
;;; lang-nim.el ends here
