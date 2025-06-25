;; ====================
;; Core UI/UX Settings
;; ====================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(global-tab-line-mode -1)

;; icons
(use-package all-the-icons :if (display-graphic-p))
;; run M-x all-the-icons-install-fonts

;; Font and Theme
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 120)
(use-package doom-themes
  :config
  (load-theme 'doom-homage-black t)
  (doom-themes-visual-bell-config))  ; Flash mode-line on error

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-minor-modes nil))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(provide 'core-ui)
