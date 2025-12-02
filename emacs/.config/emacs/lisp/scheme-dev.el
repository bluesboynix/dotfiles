;;; scheme-dev.el --- Scheme Development Config (Guile Scheme) -*- lexical-binding: t; -*-

;; -----------------------------
;; Smartparens
;; -----------------------------
(use-package smartparens
  :hook ((emacs-lisp-mode
          lisp-mode
          slime-repl-mode
          scheme-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil))

;; -----------------------------
;; Eldoc + Hover Docs
;; -----------------------------
(use-package eldoc-box
  :hook (scheme-mode . eldoc-box-hover-mode))

(add-hook 'scheme-mode-hook #'eldoc-mode)

;; -----------------------------
;; Macro Expansion
;; -----------------------------
(use-package macrostep
  :after scheme
  :bind (:map scheme-mode-map
              ("C-c e" . macrostep-expand)))

;; -----------------------------
;; Rainbow Delimiters (custom colors)
;; -----------------------------
(use-package rainbow-delimiters
  :hook ((prog-mode
          lisp-mode
          emacs-lisp-mode
          scheme-mode
          scheme-ts-mode
          sly-mrepl-mode
          slime-repl-mode) . rainbow-delimiters-mode)
  :config
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-ts-mode-hook #'rainbow-delimiters-mode)

  (add-hook
   'emacs-startup-hook
   (lambda ()
     (with-eval-after-load 'rainbow-delimiters
       (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FF0000")
       (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FF8C00")
       (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#FFFF00")
       (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#00FF00")
       (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#56B6C2")
       (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#9467BD")
       (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#D19A66")
       (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                           :foreground "white" :background "#FF0000" :weight 'bold)))))

;; -----------------------------
;; Corfu Completion
;; -----------------------------
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 4)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

;; Cape extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;; -----------------------------
;; Guile Scheme REPL (Comint)
;; -----------------------------
(defun run-guile ()
  "Run Guile REPL in a right-side split."
  (interactive)
  (unless (comint-check-proc "*guile*")
    (set-buffer (make-comint "guile" "/usr/bin/guile")))
  (let ((repl-buffer (get-buffer "*guile*"))
        (cur-window (selected-window)))
    (unless (get-buffer-window repl-buffer)
      (let ((new-window (split-window-right)))
        (set-window-buffer new-window repl-buffer)))
    (select-window cur-window)))

(defun guile--send (string)
  "Low-level sender for Guile REPL."
  (unless (comint-check-proc "*guile*")
    (set-buffer (make-comint "guile" "/usr/bin/guile")))
  (let ((proc (get-buffer-process "*guile*")))
    (comint-send-string proc (concat string "\n"))))

(defun guile-send-region (start end)
  "Send region to Guile REPL."
  (interactive "r")
  (guile--send (buffer-substring-no-properties start end)))

(defun guile-send-buffer ()
  "Send whole buffer to Guile."
  (interactive)
  (guile-send-region (point-min) (point-max)))

(defun guile-send-definition ()
  "Send current definition to Guile."
  (interactive)
  (save-excursion
    (mark-defun)
    (guile-send-region (region-beginning) (region-end)))
  (deactivate-mark))

(defun guile-clear-repl ()
  "Clear Guile REPL buffer."
  (interactive)
  (with-current-buffer "*guile*"
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") #'guile-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-r") #'guile-send-region)
  (define-key scheme-mode-map (kbd "C-c C-b") #'guile-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-z") #'run-guile)
  (define-key scheme-mode-map (kbd "C-c C-l") #'guile-clear-repl))


(provide 'scheme-dev)
;;; scheme-dev.el ends here
