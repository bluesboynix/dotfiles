;; ;;; tools-vterm.el --- Terminal inside Emacs with vterm-toggle -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :commands vterm
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (setq-local global-hl-line-mode nil)))
  :config
  ;; Better performance
  (setq vterm-max-scrollback 10000)

  ;; Optional: make vterm open in project root
  (setq vterm-toggle-cd-auto-create-buffer t))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  ;; Open vterm in a side window (bottom)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (setq vterm-toggle-use-dedicated-buffer t)
  (setq vterm-toggle-hide-method 'delete-window))

(global-set-key [f9] 'vterm-toggle)
(global-set-key [C-f9] 'vterm-toggle-cd)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))


(provide 'tools-vterm)
;;; tools-vterm.el ends here
