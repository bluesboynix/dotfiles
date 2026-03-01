;;; tools-git.el --- Minimal Git integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal, stable Git setup for Emacs 30+
;; Uses Magit + diff-hl only.

;;; Code:

;; --------------------------------------------------
;; Magit (main Git interface)
;; --------------------------------------------------

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; --------------------------------------------------
;; diff-hl (show git changes in fringe)
;; --------------------------------------------------

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

(message "Minimal Git setup loaded.")
(provide 'tools-git)
;;; tools-git.el ends here
