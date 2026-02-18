;;; ui-base.el --- Basic UI tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Core UI configuration including themes, fonts, modeline, and completion.

;;; Code:

;; ----------------------------------------------------------------------
;; Startup & Frame
;; ----------------------------------------------------------------------
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; ----------------------------------------------------------------------
;; Scratch buffer
;; ----------------------------------------------------------------------
(setq initial-scratch-message "")        ;; Empty scratch buffer

;; ----------------------------------------------------------------------
;; Editing Basics
;; ----------------------------------------------------------------------
(setq-default indent-tabs-mode nil
              tab-width 2)
(show-paren-mode 1)
(column-number-mode 1)

;; ----------------------------------------------------------------------
;; File Handling
;; ----------------------------------------------------------------------
(setq make-backup-files nil
      auto-revert-avoid-polling t
      auto-revert-interval 5)
(save-place-mode 1)
(global-auto-revert-mode 1)

;; ----------------------------------------------------------------------
;; Visual Feedback
;; ----------------------------------------------------------------------
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Disable line numbers in certain modes
(dolist (mode '(shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                pdf-view-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ----------------------------------------------------------------------
;; Mode Line Customization
;; ----------------------------------------------------------------------
(setq mode-line-position-column-line-format '(" (%l,%c)")  ; Show line and column
      mode-line-percent-position '(-3 "%p"))               ; Show percentage

;; ----------------------------------------------------------------------
;; Theme
;; ----------------------------------------------------------------------
;; Load the custom Genesis Dark theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(use-package ef-themes
  :ensure t)

;; Load theme safely
(defun my/load-theme-safely (theme)
  "Load THEME with error handling."
  (condition-case err
      (load-theme theme t)
    (error
     (message "Failed to load theme %s: %s" theme err)
     (load-theme 'ef-dark t))))  ; Fallback to default

(my/load-theme-safely 'ef-dark)

;; ----------------------------------------------------------------------
;; Icons
;; ----------------------------------------------------------------------
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; ----------------------------------------------------------------------
;; Modeline
;; ----------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-enable-word-count t
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-github (executable-find "gh")  ; Only if gh CLI exists
        doom-modeline-env-version t
        doom-modeline-height (if (>= (frame-height) 60) 
                                 my-ui-modeline-height 
                               15)
        doom-modeline-bar-width 3)
  :hook (after-init . doom-modeline-mode))

;; ----------------------------------------------------------------------
;; Minibuffer & Completion
;; ----------------------------------------------------------------------
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)  ; Enable cycling through candidates
  :init
  (vertico-mode))

;; Optionally add vertico-directory for better directory navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (orderless-component-separator " ")  ; Space for multiple patterns (AND)
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; ----------------------------------------------------------------------
;; Window Management
;; ----------------------------------------------------------------------
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Add window resizing
(global-set-key (kbd "C-c C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-c C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-<up>")    'shrink-window)
(global-set-key (kbd "C-c C-<down>")  'enlarge-window)

;; ----------------------------------------------------------------------
;; Startup Performance
;; ----------------------------------------------------------------------
;; Visual feedback during startup
(defvar my/startup-time (current-time))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %.2f seconds"
                     (float-time (time-subtract (current-time) my/startup-time)))))

;; ----------------------------------------------------------------------
;; Final message
;; ----------------------------------------------------------------------
(message "UI base loaded successfully.")

(provide 'ui-base)
;;; ui-base.el ends here
