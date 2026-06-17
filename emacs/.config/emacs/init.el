;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; High-performance GC tuning (16GB RAM machine)
;; Very high during startup
(setq gc-cons-threshold (* 300 1024 1024))
(setq gc-cons-percentage 0.6)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-package)
(require 'core-config)
(require 'core-ui)

;; tools
(require 'tool-treemacs)
(require 'tool-completions)
(require 'tool-rainbow-paren)

;; language
(require 'lang-c-cpp)
(require 'lang-go)
(require 'lang-python)
(require 'lang-rust)

(message "init.el loaded all modules successfully.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)
