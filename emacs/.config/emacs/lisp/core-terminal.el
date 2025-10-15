;;; core-terminal.el --- Terminal inside Emacs -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Optional: increase scrollback buffer
  (setq vterm-max-scrollback 10000)
  ;; Optional: split window keybindings
  (defun open-vterm-horizontal-split ()
    "Open vterm in a horizontal split."
    (interactive)
    (split-window-below)
    (other-window 1)
    (vterm))
  (defun open-vterm-vertical-split ()
    "Open vterm in a vertical split."
    (interactive)
    (split-window-right)
    (other-window 1)
    (vterm))
  ;; Example keybindings
  (global-set-key (kbd "C-c t h") 'open-vterm-horizontal-split)
  (global-set-key (kbd "C-c t v") 'open-vterm-vertical-split))

(message "vterm loaded successfully.")
(provide 'core-terminal)
