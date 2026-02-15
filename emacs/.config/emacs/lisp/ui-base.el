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
;; Custom UI Groups
;; ----------------------------------------------------------------------
(defgroup my-ui nil
  "Custom UI configurations."
  :group 'emacs)

(defcustom my-ui-font-height 120
  "Default font height."
  :type 'integer
  :group 'my-ui)

(defcustom my-ui-modeline-height 25
  "Height of the doom modeline."
  :type 'integer
  :group 'my-ui)

;; ----------------------------------------------------------------------
;; Scratch buffer
;; ----------------------------------------------------------------------
(setq initial-scratch-message "")        ;; Empty scratch buffer
(setq initial-major-mode 'lisp-interaction-mode) ;; Ensure Lisp evaluation mode

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
;; Scrolling & Display
;; ----------------------------------------------------------------------
(setq scroll-margin 5
      scroll-conservatively 999
      scroll-preserve-screen-position t
      indicate-empty-lines t
      indicate-buffer-boundaries 'left
      ring-bell-function 'ignore
      visible-bell nil
      completions-detailed t
      completions-format 'one-column)

;; ----------------------------------------------------------------------
;; Mode Line Customization
;; ----------------------------------------------------------------------
(setq mode-line-position-column-line-format '(" (%l,%c)")  ; Show line and column
      mode-line-percent-position '(-3 "%p"))               ; Show percentage

;; Unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; ----------------------------------------------------------------------
;; Theme
;; ----------------------------------------------------------------------
;; Load the custom Genesis Dark theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

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
;; Icons
;; ----------------------------------------------------------------------
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

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
;; Font Configuration
;; ----------------------------------------------------------------------
(defun my/font-available-p (font-name)
  "Check if FONT-NAME is available on the system."
  (find-font (font-spec :name font-name)))

(defun my/set-font ()
  "Set font with fallbacks for different systems."
  (interactive)
  (cond
   ((my/font-available-p "Fira Code")
    (set-face-attribute 'default nil :font "Fira Code" :height my-ui-font-height))
   ((my/font-available-p "Cascadia Code")
    (set-face-attribute 'default nil :font "Cascadia Code" :height my-ui-font-height))
   ((my/font-available-p "JetBrains Mono")
    (set-face-attribute 'default nil :font "JetBrains Mono" :height my-ui-font-height))
   ((my/font-available-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height (- my-ui-font-height 10)))
   ((my/font-available-p "Menlo")
    (set-face-attribute 'default nil :font "Menlo" :height (- my-ui-font-height 10)))
   ((my/font-available-p "Monaco")
    (set-face-attribute 'default nil :font "Monaco" :height (- my-ui-font-height 10)))
   (t
    (set-face-attribute 'default nil :font "Monospace" :height (- my-ui-font-height 10)))))

(my/set-font)

;; Set fallback fonts for better Unicode support
(when (display-graphic-p)
  (condition-case nil
      (progn
        (set-fontset-font t 'unicode (font-spec :name "Symbola") nil 'append)
        (set-fontset-font t 'unicode (font-spec :name "Noto Color Emoji") nil 'append))
    (error (message "Failed to set fallback fonts"))))

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
