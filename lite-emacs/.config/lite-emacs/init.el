;;; init.el --- Lite Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Minimal, package-free Emacs configuration.
;; Intended for writing, note-taking, and quick editing.
;;

;;; Code:

;; Faster startup
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.5)

;; Restore GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Load local modules
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; Core configuration
(require 'core-config)
(require 'core-ui)

(message "Lite Emacs loaded successfully.")

;; Keep customizations separate
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

(provide 'init)
;;; init.el ends here
