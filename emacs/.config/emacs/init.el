;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------
;; Setup load-path for custom lisp modules
;; -------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -------------------------------------------------------------------
;; Core modules
;; -------------------------------------------------------------------
(require 'core-ui)           ;; Basic UI
(require 'core-editing)      ;; Editing preferences
(require 'core-packages)     ;; Package management setup
(require 'core-utilities)    ;; Corfu, Which-Key, Projectile
(require 'core-ui-polish)    ;; Fonts, themes, modeline
(require 'core-terminal)     ;; vterm integration
(require 'core-git)          ;; Magit + diff-hl
(require 'core-dashboard)    ;; dashboard for emacs
(require 'core-file-browser) ;; use treemacs
;; -------------------------------------------------------------------
;; Language modules
;; -------------------------------------------------------------------
(require 'lang-cpp)          ;; C/C++
(require 'lang-python)       ;; Python
(require 'lang-rust)         ;; Rust

;; -------------------------------------------------------------------
;; Common Lisp
;; -------------------------------------------------------------------
(require 'common-lisp-colors) ;; Custom syntax highlighting
(require 'common-lisp-dev)    ;; SLIME, Smartparens, Eldoc, Rainbow delimiters


;; -------------------------------------------------------------------
;; Scheme -> current implementation : bigloo
;; -------------------------------------------------------------------
(require 'scheme-colors) ;; Custom syntax highlighting
(require 'scheme-dev)


;; -------------------------------------------------------------------
;; Load custom-set-variables and custom-set-faces
;; -------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; -------------------------------------------------------------------
;; Final message
;; -------------------------------------------------------------------
(message "init.el loaded all modules successfully.")
