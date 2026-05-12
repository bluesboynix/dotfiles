;;; tool-treemacs.el --- Treemacs file explorer configuration -*- lexical-binding: t; -*-

(require 'treemacs)

;; ---------- Basic Setup ----------
(setq treemacs-width 35
      treemacs-indentation 2
      treemacs-git-mode 'simple                     ; show git status
      treemacs-file-event-delay 0.5)                ; debounce auto-refresh

;; Remove mode line in treemacs buffer for cleaner look
(add-hook 'treemacs-mode-hook
          (lambda () (hide-mode-line-mode 1)))

;; Auto-refresh when files change
(treemacs-filewatch-mode t)

;; ---------- Keybindings (global) ----------
(global-set-key (kbd "C-x t t") 'treemacs)          ; toggle treemacs
(global-set-key (kbd "C-x t C-t") 'treemacs-find-file) ; jump to current file in treemacs

;; ---------- Icons in Dired (to match treemacs) ----------
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; ---------- Optional: Magit integration ----------
(use-package treemacs-magit
  :ensure t
  :after magit)

;; ---------- Optional: LSP integration (if you use lsp-mode) ----------
;; (use-package lsp-treemacs
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-treemacs-errors-list)

(provide 'tool-treemacs)
;;; tool-treemacs.el ends here
