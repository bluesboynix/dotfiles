;;; ui-centaur-tabs.el --- Minimal Centaur Tabs config -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :ensure t
  :hook (after-init . centaur-tabs-mode)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons nil)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-height 24)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-gray-out-inactive-buffers t)
  (centaur-tabs-set-close-button nil)

  :config
  ;; Simple grouping
  (setq centaur-tabs-buffer-groups-function
        (lambda ()
          (list
           (cond
            ((derived-mode-p 'prog-mode) "Code")
            ((derived-mode-p 'text-mode) "Text")
            (t "Other")))))

  ;; Keyboard only (no mouse bindings)
  (global-set-key (kbd "M-[") #'centaur-tabs-backward)
  (global-set-key (kbd "M-]") #'centaur-tabs-forward)
  )

(provide 'ui-centaur-tabs)
;;; ui-centaur-tabs.el ends here
