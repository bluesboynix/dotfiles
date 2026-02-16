;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Setup load-path for custom lisp modules
;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setenv "PATH" (concat (expand-file-name "~/.cargo/bin") ":" (getenv "PATH")))

(add-to-list 'exec-path (expand-file-name "~/.nimble/bin"))
(setenv "PATH"
        (concat (expand-file-name "~/.nimble/bin") ":"
                (getenv "PATH")))

;; Terminal-specific fixes
(when (not (display-graphic-p))
  ;; Fix for stray characters in terminal
  (add-hook 'after-init-hook
            (lambda ()
              ;; Disable problematic modes in terminal
              (when (bound-and-true-p cua-mode)
                (cua-mode -1))
              
              ;; Better terminal handling
              (setq-local read-process-output-max (* 1024 1024))
              
              ;; Handle terminal escape sequences properly
              (define-key input-decode-map (kbd "O") [O])
              (define-key input-decode-map (kbd "I") [I])
              
              ;; If using Corfu in terminal, ensure it's properly disabled
              (when (fboundp 'corfu-terminal-mode)
                (corfu-terminal-mode -1))
              
              ;; Disable mouse in terminal
              (xterm-mouse-mode -1)
              
              ;; Ensure proper terminal encoding
              (prefer-coding-system 'utf-8-unix)
              (set-terminal-coding-system 'utf-8-unix)
              (set-keyboard-coding-system 'utf-8-unix)
              
              (message "Terminal-specific fixes applied"))))



;; Core modules
(require 'ui-base)           ;; Basic UI
(require 'core-packages)     ;; Package management setup
(require 'utils-collection)  ;; Corfu, Which-Key, Projectile
(require 'ui-dashboard)      ;; dashboard for emacs
(require 'ui-centaur-tabs)   ;; fancy tabbar

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
(require 'lang-nim)
(require 'lang-python)
(require 'lang-python-colors)
(require 'lang-rust)
(require 'lang-rust-colors)
; (require 'lang-common-lisp-colors)
; (require 'lang-common-lisp)
(require 'lang-scheme-colors)
(require 'lang-scheme)
(require 'lang-racket-colors)
(require 'lang-racket)
(require 'lang-flutter-dart)
(require 'lang-go)

;; Load custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
(setq bracketed-paste-enable nil)

;; Final message
(message "init.el loaded all modules successfully.")
;;; init.el ends here
