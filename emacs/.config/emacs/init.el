;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; --------------------------------------------------
;; High-performance GC tuning (16GB RAM machine)
;; --------------------------------------------------

;; Very high during startup
(setq gc-cons-threshold (* 300 1024 1024))
(setq gc-cons-percentage 0.6)

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

;; UI Layer
(require 'ui-base)
(require 'ui-rainbow-delimiters)

;; Dev Layer
(require 'dev-core)
(require 'dev-lsp)
(require 'dev-diagnostics)
(require 'dev-format)
(require 'dev-project)

;; Tools Layer
(require 'tools-company)
(require 'tools-orderless)
(require 'tools-vertico-stack)
(require 'tools-smartparens)
(require 'tools-vterm)
(require 'tools-git)
(require 'tools-treemacs)
(require 'tools-dired)
(require 'tools-snippets)

;; Languages
(require 'lang-cpp)
(require 'lang-go)



;; Load custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
(setq bracketed-paste-enable nil)

;; Final message
(message "init.el loaded all modules successfully.")

;;; init.el ends here
