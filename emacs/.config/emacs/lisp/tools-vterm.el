;;; tools-vterm.el --- Terminal inside Emacs with vterm-toggle -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :commands vterm)

(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind
  :config
  )

(global-set-key [f9] 'vterm-toggle)
(global-set-key [C-f9] 'vterm-toggle-cd)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))


(provide 'tools-vterm)
;;; core-terminal.el  ends here
