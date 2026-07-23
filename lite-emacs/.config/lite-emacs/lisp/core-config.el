;;; core-config.el --- Built-in Emacs settings (no packages) -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(electric-pair-mode 1)
(setq use-short-answers t)
(setq make-backup-files nil)
(setq auto-save-default nil)      ; optional: disable auto-save files too
(setq create-lockfiles nil)       ; disable .# lock files
(global-auto-revert-mode 1)
(setq scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position t)
(show-paren-mode 1)
(setq search-upper-case t)
(setq isearch-allow-scroll t)   ; allow scrolling during search
(setq ring-bell-function #'ignoure)
(prefer-coding-system 'utf-8)

(provide 'core-config)
;;; core-config.el ends here
