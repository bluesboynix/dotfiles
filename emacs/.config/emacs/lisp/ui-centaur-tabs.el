;;; ui-centaur-tabs.el --- Centaur Tabs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern tab bar using centaur-tabs.
;; Provides buffer grouping and visual enhancements.

;;; Code:

(use-package centaur-tabs
  :ensure t
  :demand t
  :hook
  (after-init . centaur-tabs-mode)

  :custom
  ;; Basic behavior
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-height 32)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-cycle-scope 'tabs)

  :config

  ;; Enable tab mode
  (centaur-tabs-mode t)

  ;; Group buffers
  (defun centaur-tabs-buffer-groups ()
    "Organize buffers into groups."
    (list
     (cond
      ((or (string-prefix-p "*" (buffer-name))
           (memq major-mode '(help-mode
                              helpful-mode
                              completion-list-mode)))
       "System")

      ((derived-mode-p 'prog-mode)
       "Code")

      ((derived-mode-p 'text-mode)
       "Text")

      ((derived-mode-p 'dired-mode)
       "Dired")

      (t
       "User")))))

;; Keybindings
(global-set-key (kbd "M-[") #'centaur-tabs-backward)
(global-set-key (kbd "M-]") #'centaur-tabs-forward)
(global-set-key (kbd "M-{") #'centaur-tabs-move-current-tab-to-left)
(global-set-key (kbd "M-}")  #'centaur-tabs-move-current-tab-to-right)

(provide 'ui-centaur-tabs)
;;; ui-centaur-tabs.el ends here
