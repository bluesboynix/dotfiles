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
(setq-default tab-width 2)

;; electric pair mode
(electric-pair-mode 1)

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

;; Isearch: case-insensitive by default, but sensitive when uppercase used
(setq search-upper-case t)
(setq isearch-allow-scroll t)   ; allow scrolling during search

;; File encoding (avoid Unicode issues)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; Set the default column position (e.g., column 80)
(setq-default fill-column 80)
;; Enable the vertical line indicator globally
;;(global-display-fill-column-indicator-mode 1)

;; custom emacs focus on new split buffer
(defun my/split-window-right-and-follow ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun my/split-window-below-and-follow ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(global-set-key (kbd "C-x 3") #'my/split-window-right-and-follow)
(global-set-key (kbd "C-x 2") #'my/split-window-below-and-follow)

;; Window resizing - C-c w h/l = horizontal and C-c w j/k = vertical
(defvar my-window-resize-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'shrink-window-horizontally)
    (define-key map (kbd "l") #'enlarge-window-horizontally)
    (define-key map (kbd "j") #'shrink-window)
    (define-key map (kbd "k") #'enlarge-window)
    map))

(global-set-key (kbd "C-c w") my-window-resize-map)

;; Keep the resize keys active so h/j/k/l can be pressed repeatedly.
(repeat-mode 1)

(put 'shrink-window-horizontally 'repeat-map 'my-window-resize-map)
(put 'enlarge-window-horizontally 'repeat-map 'my-window-resize-map)
(put 'shrink-window 'repeat-map 'my-window-resize-map)
(put 'enlarge-window 'repeat-map 'my-window-resize-map)

(provide 'core-config)
;;; core-config.el ends here
