;;; core-ui.el --- Basic UI tweaks -*- lexical-binding: t; -*-

;; Scratch buffer settings
(setq initial-scratch-message "")        ;; Empty scratch buffer
(setq initial-major-mode 'lisp-interaction-mode) ;; Ensure Lisp evaluation mode

;; Disable startup screen and GUI elements
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show line numbers and highlight current line
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

(provide 'core-ui)
