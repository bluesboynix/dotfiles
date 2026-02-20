;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; --------------------------------------------------
;; High-performance GC tuning (16GB RAM machine)
;; --------------------------------------------------

;; Very high during startup
(setq gc-cons-threshold (* 200 1000 1000)) ;; 200MB
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Lower but still generous during normal usage
            (setq gc-cons-threshold (* 50 1000 1000)) ;; 50MB
            (setq gc-cons-percentage 0.1)))


;; Setup load-path for custom lisp modules
;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.config/emacs/lisp/")
;; (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;; (setenv "PATH" (concat (expand-file-name "~/.cargo/bin") ":" (getenv "PATH")))

;; (add-to-list 'exec-path (expand-file-name "~/.nimble/bin"))
;; (setenv "PATH"
;;        (concat (expand-file-name "~/.nimble/bin") ":"
;;                (getenv "PATH")))


(require 'core-packages)
(require 'ui-base)
(require 'ui-rainbow-delimiters)

;; Tools and Extra
(require 'tools-company)
(require 'tools-orderless)
(require 'tools-vertico-stack)
(require 'tools-smartparens)
(require 'tools-vterm)
(require 'tools-git)
(require 'tools-treemacs)
(require 'tools-dired)
(require 'tools-snippets)

;; Language modules
(require 'lang-cpp)
(require 'lang-go)
;; (require 'lang-html)
;; (require 'lang-css)
;; (require 'lang-javascript)

;; Load custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
(setq bracketed-paste-enable nil)

;; Final message
(message "init.el loaded all modules successfully.")
;;; init.el ends here
