;;; init.el --- Entry point -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)
            (setq gc-cons-threshold (* 2 1000 1000))))



(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules
(require 'packages)
(require 'core-ui)
(require 'editing)
(require 'keybindings)
(require 'completion)
(require 'lsp-config)
(require 'lisp-dev)
(require 'syntax-highlighting)
(require 'extras)
(require 'scheme-dev.el)
(require 'syntax-highlighting-scheme)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(unless (server-running-p) (server-start))

(provide 'init)
;;; init.el ends here
