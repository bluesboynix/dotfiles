;;; lang-cpp.el --- C/C++ IDE setup -*- lexical-binding: t; -*-

;; Built-in editing mode for C/C++
(use-package cc-mode
  :ensure nil
  :hook ((c-mode . (lambda () (setq c-basic-offset 4)))
         (c++-mode . (lambda () (setq c-basic-offset 4)))))

;; LSP with Eglot
(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (setq eglot-send-changes-idle-time 0.5))

;; Syntax checking
(use-package flymake
  :hook (c-mode . flymake-mode)
  :hook (c++-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

(message "C/C++ IDE module loaded successfully.")
(provide 'lang-cpp)
