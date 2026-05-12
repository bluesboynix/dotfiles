;;; core-config.el --- Built-in Emacs settings (no packages) -*- lexical-binding: t; -*-

;; UI cleanup
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Line & column display
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Indentation: spaces, no tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Yes/no prompts -> y/n
(setq use-short-answers t)

;; No automatic backup files (redundant with git)
(setq make-backup-files nil)
(setq auto-save-default nil)      ; optional: disable auto-save files too
(setq create-lockfiles nil)       ; disable .# lock files

;; Auto-revert buffers when files change externally
(global-auto-revert-mode 1)

;; Better scrolling
(setq scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Show parentheses matching
(show-paren-mode 1)

(provide 'core-config)
;;; core-config.el ends here
