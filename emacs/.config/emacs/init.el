;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; Setup load-path for custom lisp modules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setenv "PATH" (concat (expand-file-name "~/.cargo/bin") ":" (getenv "PATH")))

;; Core modules
(require 'ui-base)           ;; Basic UI
(require 'core-packages)     ;; Package management setup
(require 'utils-collection)  ;; Corfu, Which-Key, Projectile
(require 'ui-dashboard)      ;; dashboard for emacs

;; Tools and Extra
(require 'extra-emms)        ;; Emacs Music
(require 'tools-pdf)         ;; view pdf in emacs
(require 'tools-vterm)       ;; vterm integration
(require 'tools-git)         ;; Magit + diff-hl
(require 'tools-treemacs)    ;; use treemacs
(require 'tools-dired)       ;; dired config

;; Language modules
(require 'lang-cpp)      
(require 'lang-cpp-colors)
(require 'lang-python)
(require 'lang-python-colors)
(require 'lang-rust)
(require 'lang-rust-colors)
(require 'lang-common-lisp-colors)
(require 'lang-common-lisp)
(require 'lang-scheme-colors)
(require 'lang-scheme)
(require 'lang-racket-colors)
(require 'lang-racket)

;; Load custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Final message
(message "init.el loaded all modules successfully.")
