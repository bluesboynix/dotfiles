;;; lang-python.el --- Python IDE setup -*- lexical-binding: t; -*-

;; Python major mode
(use-package python
  :ensure t
  :hook (python-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (setq python-indent-offset 4))))

;; LSP for Python
(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.5))

;; Syntax checking with Flymake
(use-package flymake
  :hook (python-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

;; Optional: format Python with black on save
(use-package python-black
  :ensure t
  :hook (python-mode . python-black-on-save-mode))

(message "Python IDE module loaded successfully.")
(provide 'lang-python)
