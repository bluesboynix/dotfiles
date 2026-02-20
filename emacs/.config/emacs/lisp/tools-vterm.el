;;; tools-vterm.el --- Terminal inside Emacs with vterm-toggle -*- lexical-binding: t; -*-
;;; Commentary:
;; Terminal emulator with vterm and side-window toggling.

;;; Code:

(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :hook (vterm-mode . (lambda ()
    (display-line-numbers-mode -1)
  (setq-local global-hl-line-mode nil)))
  :custom ;; Better performance
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :config ;; Optional: make vterm open in project root
  (setq vterm-toggle-cd-auto-create-buffer t))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :defer t
  :bind (("<f9>" . vterm-toggle) ("C-<f9>" . vterm-toggle-cd))
  :custom ;; Open vterm in a side window (bottom)
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  (vterm-toggle-use-dedicated-buffer t)
  (vterm-toggle-hide-method 'delete-window)
  (vterm-toggle-position 'bottom)
  (vterm-toggle-size 15))

(defun my/vterm-here ()
  "Open vterm in current buffer's directory."
  (interactive)
  (let ((default-directory (or
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory))))
    (vterm-toggle)))

    (global-set-key (kbd "C-c t h") #'my/vterm-here)

(message "VTerm configuration loaded. Press F9 to toggle terminal.") (

provide 'tools-vterm)
;;; tools-vterm.el ends here
