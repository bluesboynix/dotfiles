;;; ui-centaur-tabs.el --- Centaur Tabs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern tab bar using centaur-tabs with flat browser-like appearance.
;; Uses Meta keybindings (not browser-style).

;;; Code:

(use-package centaur-tabs
  :ensure t
  :demand t
  :hook
  (after-init . centaur-tabs-mode)
  :custom
  ;; Flat style (no gradient/3D effect)
  (centaur-tabs-style "chamfer")  ; Flat appearance
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-height 32)
  (centaur-tabs-cycle-scope 'tabs)

  ;; Visual enhancements for flat look
  (centaur-tabs-gray-out-inactive-buffers t)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-close-button "x")
  (centaur-tabs-set-close-unsaved-icon t)
  (centaur-tabs-modified-marker "●")  ; Dot for modified indicator
  
  :config
  ;; Create a completely flat tab appearance
  (defun my/centaur-tabs-flat-style ()
    "Apply flat browser-like style to centaur tabs."
    ;; Remove all borders and gradients
    (set-face-attribute 'centaur-tabs-default nil
                        :background "#1e1e2e"
                        :foreground "#cdd6f4"
                        :underline nil
                        :box nil
                        :slant 'normal
                        :weight 'normal
                        :height 1.0)

    (set-face-attribute 'centaur-tabs-unselected nil
                        :background "#1e1e2e"
                        :foreground "#6c7086"
                        :underline nil
                        :box nil
                        :slant 'normal
                        :weight 'normal
                        :height 1.0)

    (set-face-attribute 'centaur-tabs-selected nil
                        :background "#313244"  ; Slightly lighter for active tab
                        :foreground "#ffffff"
                        :underline nil
                        :box nil
                        :slant 'normal
                        :weight 'normal
                        :height 1.0)

    (set-face-attribute 'centaur-tabs-selected-modified nil
                        :background "#313244"
                        :foreground "#f9e2af"  ; Yellow for modified
                        :underline nil
                        :box nil
                        :slant 'normal
                        :weight 'normal
                        :height 1.0)

    (set-face-attribute 'centaur-tabs-unselected-modified nil
                        :background "#1e1e2e"
                        :foreground "#f9e2af"
                        :underline nil
                        :box nil
                        :slant 'normal
                        :weight 'normal
                        :height 1.0)

    ;; For versions that have active-bar face, try to set it if it exists
    (when (facep 'centaur-tabs-active-bar)
      (set-face-attribute 'centaur-tabs-active-bar nil
                          :background "#89b4fa"
                          :foreground "#89b4fa"
                          :height 0.1))

    ;; Make the close button more subtle
    (when (facep 'centaur-tabs-close-selected)
      (set-face-attribute 'centaur-tabs-close-selected nil
                          :background "#313244"
                          :foreground "#f38ba8"
                          :box nil))

    (when (facep 'centaur-tabs-close-unselected)
      (set-face-attribute 'centaur-tabs-close-unselected nil
                          :background "#1e1e2e"
                          :foreground "#6c7086"
                          :box nil)))

  ;; Apply the flat style
  (my/centaur-tabs-flat-style)

  ;; Advanced tab grouping (your original)
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
       "User"))))

  ;; Hide certain buffers from tabs (optional enhancement)
  (defun my/centaur-tabs-hide-buffer-p (buffer)
    "Hide BUFFER from tabs if it matches certain patterns."
    (with-current-buffer buffer
      (or (string-prefix-p " " (buffer-name))
          (string-prefix-p "*Messages*" (buffer-name))
          (string-prefix-p "*Compile-Log*" (buffer-name))
          (string-prefix-p "*Warnings*" (buffer-name))
          (string-prefix-p "*straight*" (buffer-name))
          (string-prefix-p "*Backtrace*" (buffer-name))
          (minibufferp buffer))))

  (setq centaur-tabs-hide-buffer-predicate #'my/centaur-tabs-hide-buffer-p)

  ;; Enable tab mode
  (centaur-tabs-mode t)

  ;; Original keybindings (your preferred Meta-based ones)
  (global-set-key (kbd "M-[") #'centaur-tabs-backward)
  (global-set-key (kbd "M-]") #'centaur-tabs-forward)
  (global-set-key (kbd "M-{") #'centaur-tabs-move-current-tab-to-left)
  (global-set-key (kbd "M-}") #'centaur-tabs-move-current-tab-to-right)

  ;; Mouse support (optional enhancement)
  (global-set-key [mode-line mouse-1] #'centaur-tabs-forward)
  (global-set-key [mode-line mouse-3] #'centaur-tabs-backward)
  (global-set-key [mode-line mouse-2] #'kill-this-buffer)

  ;; Optional: Keep the tab management commands if you want them
  (defun my/centaur-tabs-new-tab ()
    "Create a new tab with scratch buffer."
    (interactive)
    (let ((buffer (generate-new-buffer "*scratch*")))
      (switch-to-buffer buffer)
      (lisp-interaction-mode)))

  (defun my/centaur-tabs-kill-other-tabs ()
    "Kill all tabs except current."
    (interactive)
    (let ((current (current-buffer)))
      (dolist (buf (buffer-list))
        (unless (or (eq buf current)
                    (string-prefix-p "*" (buffer-name buf))
                    (minibufferp buf))
          (kill-buffer buf)))))

  ;; Additional commands with leader prefix (optional)
  (global-set-key (kbd "C-c t n") #'my/centaur-tabs-new-tab)     ; New tab
  (global-set-key (kbd "C-c t k") #'kill-this-buffer)            ; Kill current
  (global-set-key (kbd "C-c t o") #'my/centaur-tabs-kill-other-tabs)  ; Kill others
  (global-set-key (kbd "C-c t g") #'centaur-tabs-group-by-project)    ; Group by project
  (global-set-key (kbd "C-c t r") #'centaur-tabs-reset-groups)        ; Reset groups
  )

(provide 'ui-centaur-tabs)
;;; ui-centaur-tabs.el ends here
