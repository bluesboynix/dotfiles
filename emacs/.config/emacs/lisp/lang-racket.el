;;; lang-racket.el --- Racket Development Config -*- lexical-binding: t; -*-

;; -----------------------------
;; Smartparens
;; -----------------------------
(use-package smartparens
  :hook ((emacs-lisp-mode
          lisp-mode
          racket-mode
          racket-repl-mode) . smartparens-mode)
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
  :hook (racket-mode . eldoc-box-hover-mode))

(add-hook 'racket-mode-hook #'eldoc-mode)
(add-hook 'racket-repl-mode-hook #'eldoc-mode)

;; -----------------------------
;; Macro Expansion
;; -----------------------------
(use-package macrostep
  :after racket-mode
  :bind (:map racket-mode-map
              ("C-c e" . macrostep-expand)))

;; -----------------------------
;; Rainbow Delimiters (custom colors)
;; -----------------------------
(use-package rainbow-delimiters
  :hook ((prog-mode
          lisp-mode
          emacs-lisp-mode
          racket-mode
          racket-repl-mode
          scheme-mode
          scheme-ts-mode
          slime-repl-mode
          sly-mrepl-mode) . rainbow-delimiters-mode)
  :config
  (add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)

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

;; CAPE completion extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; -----------------------------
;; Racket Mode + REPL Integration
;; -----------------------------
(use-package racket-mode
  :hook (racket-mode . racket-xp-mode)
  :config
  ;; Matching your Scheme REPL keybindings
  (define-key racket-mode-map (kbd "C-c C-z") #'racket-run)
  (define-key racket-mode-map (kbd "C-c C-c") #'racket-send-definition)
  (define-key racket-mode-map (kbd "C-c C-r") #'racket-send-region)
  (define-key racket-mode-map (kbd "C-c C-b") #'racket-run-and-switch)
  (define-key racket-mode-map (kbd "C-c M-o") #'racket-repl-clear-leaving-last-prompt))

;; --- Compatibility helpers to match your Guile workflow ---

(defun racket-send-region (start end)
  "Send region to Racket REPL."
  (interactive "r")
  (racket-repl)
  (racket-repl-send-region start end))

(defun racket-send-definition ()
  "Send current definition to Racket REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (racket-send-region (region-beginning) (region-end)))
  (deactivate-mark))

(defun racket-run-and-switch ()
  "Run the whole buffer and jump to REPL."
  (interactive)
  (racket-run)
  (racket-repl))

(defun racket-clear-repl ()
  "Clear the Racket REPL buffer."
  (interactive)
  (let ((buf (get-buffer "*Racket REPL*")))
    (when buf
      (with-current-buffer buf
        (let ((comint-buffer-maximum-size 0))
          (comint-truncate-buffer))))))

(provide 'lang-racket)
;;; racket-dev.el ends here
