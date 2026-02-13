;;; lang-nim.el --- Nim support for Emacs 30+ -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern Nim setup using:
;; - nim-mode
;; - eglot (built-in LSP)
;; - flymake (built-in)
;; - completion-at-point
;; - nimpretty format on save

;;; Code:

;; --------------------------------------------------
;; nim-mode
;; --------------------------------------------------

(use-package nim-mode
  :ensure t
  :mode ("\\.nim\\'" "\\.nims\\'" "\\.nimble\\'")
  :interpreter "nim"
  :hook
  (nim-mode . lang-nim-setup)
  :config
  (setq nim-indent-offset 2))

;; --------------------------------------------------
;; Common setup
;; --------------------------------------------------

(defun lang-nim-setup ()
  "Common Nim configuration."
  (setq-local indent-tabs-mode nil
              tab-width 2)

  ;; Flymake (built-in diagnostics)
  (flymake-mode 1)

  ;; Eldoc
  (eldoc-mode 1)

  ;; Start eglot if available
  (when (executable-find "nimlangserver")
    (eglot-ensure)))

;; --------------------------------------------------
;; Eglot LSP
;; --------------------------------------------------

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nim-mode . ("nimlangserver"))))

;; --------------------------------------------------
;; Format on save
;; --------------------------------------------------

(defun lang-nim-format-buffer ()
  "Format current Nim buffer using nimpretty."
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

(with-eval-after-load 'nim-mode
  (define-key nim-mode-map (kbd "C-c C-b") #'lang-nim-build)
  (define-key nim-mode-map (kbd "C-c C-r") #'lang-nim-run)
  (define-key nim-mode-map (kbd "C-c C-t") #'lang-nim-test)
  (define-key nim-mode-map (kbd "C-c C-f") #'lang-nim-format-buffer))

;; --------------------------------------------------

(provide 'lang-nim)
;;; lang-nim.el ends here
