;;; init.el --- Emacs entry point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This is the main entry point of the Emacs configuration.
;; It sets up garbage collection behavior, module loading, user settings,
;; and starts the Emacs server if needed.
;;
;;; Code:

;; --------------------------------------------------
;; Optimize GC during startup, then reset afterwards
;; --------------------------------------------------
(setq gc-cons-threshold (* 50 1000 1000))  ; Increase GC threshold during startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))  ; Lower it back for normal use
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;; --------------------------------------------------
;; Load additional Lisp files from ~/emacs.d/lisp/
;; --------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; --------------------------------------------------
;; Load core configuration modules
;; --------------------------------------------------
(require 'packages)              ;; Package setup (straight, use-package, etc.)
(require 'core-ui)               ;; UI/UX: fonts, themes, modeline, dashboard
(require 'editing)               ;; General editing tweaks
(require 'keybindings)           ;; Custom keybindings
(require 'completion)            ;; Completion framework (vertico, corfu, etc.)
(require 'lsp-config)            ;; LSP setup for various languages
(require 'lisp-dev)              ;; Common Lisp/Emacs Lisp tools
(require 'syntax-highlighting)  ;; Tree-sitter or font-lock enhancements
(require 'extras)                ;; Miscellaneous helpers and UX improvements

;; --------------------------------------------------
;; Separate custom.el file (GUI-driven customization)
;; --------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------------------------------
;; Start Emacs server for emacsclient
;; --------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
