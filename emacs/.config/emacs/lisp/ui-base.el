;;; ui-base.el --- Basic UI tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure UI configuration only.
;; No completion, no dev behavior.

;;; Code:

;; ------------------------------------------------------------
;; Startup & Frame
;; ------------------------------------------------------------

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq initial-scratch-message "")

(show-paren-mode 1)
(column-number-mode 1)
(display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; ------------------------------------------------------------
;; File Handling (UI safe)
;; ------------------------------------------------------------

(setq make-backup-files nil
      auto-revert-avoid-polling t
      auto-revert-interval 5)

(save-place-mode 1)
(global-auto-revert-mode 1)

;; ------------------------------------------------------------
;; Theme
;; ------------------------------------------------------------

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(defun my/load-theme-safely (theme)
  (condition-case err
      (load-theme theme t)
    (error
     (message "Failed to load theme %s: %s" theme err)
     (load-theme 'ef-dark t))))

(use-package ef-themes
  :ensure t)

(my/load-theme-safely 'ef-dark)

(add-to-list 'default-frame-alist '(background-color . "#030303"))

;; ------------------------------------------------------------
;; Modeline
;; ------------------------------------------------------------

(use-package doom-modeline
  :init
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-enable-word-count nil
        doom-modeline-persp-name nil
        doom-modeline-lsp nil
        doom-modeline-github nil
        doom-modeline-env-version nil
        doom-modeline-height 15
        doom-modeline-bar-width 3)
  :hook (after-init . doom-modeline-mode))

;; ------------------------------------------------------------
;; Window movement
;; ------------------------------------------------------------

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;; Visual feedback during startup
(defvar my/startup-time (current-time))
(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs started in %.2f seconds"
                              (float-time
                               (time-subtract
                                (current-time) my/startup-time)))))


(provide 'ui-base)
;;; ui-base.el ends here
