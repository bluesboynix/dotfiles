;;; core-editing.el --- Basic editing behavior -*- lexical-binding: t; -*-

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Show matching parentheses
(show-paren-mode 1)

;; Save cursor position and auto-revert buffers
(save-place-mode 1)
(global-auto-revert-mode 1)

;; Disable backup files like file.txt~
(setq make-backup-files nil)

(provide 'core-editing)

