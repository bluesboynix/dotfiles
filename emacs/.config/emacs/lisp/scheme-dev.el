;;; scheme-dev.el --- Scheme Development Config (CHICKEN Scheme) -*- lexical-binding: t; -*-

;; -----------------------------
;; Smartparens
;; -----------------------------
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode scheme-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil))

;; -----------------------------
;; Eldoc and Hover Docs
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
;; Rainbow Delimiters (robust)
;; -----------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode
          lisp-mode
          emacs-lisp-mode
          scheme-mode
          scheme-ts-mode
          sly-mrepl-mode
          slime-repl-mode)
         . rainbow-delimiters-mode)
  :config
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-ts-mode-hook #'rainbow-delimiters-mode)
  ;; Custom colors (Doom One inspired)
  (add-hook 'emacs-startup-hook
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
;; Corfu for completion
;; -----------------------------
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Auto-popup completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 4)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

;; Optional: Cape for extra completion sources
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; -----------------------------
;; Geiser + CHICKEN integration
;; -----------------------------
;; (use-package geiser
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq geiser-active-implementations '(chicken)))

;; (use-package geiser-chicken
;;   :ensure t
;;   :after geiser
;;   :config
;;   (setq geiser-chicken-binary "/usr/bin/chicken-csi"))

;; Keybindings (Geiser already provides these):
;; C-c C-c — Eval definition
;; C-c C-r — Eval region
;; C-c C-b — Eval buffer
;; C-c C-z — Switch to REPL
;; C-x C-e — Eval last expression

;; -----------------------------
;; Fallback: CHICKEN Scheme (Comint Mode)
;; ----------------------------

(defun run-chicken ()
  "Run CHICKEN Scheme REPL (csi) in a vertical split, keeping focus on the code buffer."
  (interactive)
  (unless (comint-check-proc "*chicken*")
    (set-buffer (make-comint "chicken" "/usr/bin/chicken-csi")))
  (let ((repl-buffer (get-buffer "*chicken*"))
        (cur-window (selected-window)))
    (unless (get-buffer-window repl-buffer)
      ;; Split current window vertically (code left, REPL right)
      (let ((new-window (split-window-right)))
        (set-window-buffer new-window repl-buffer)))
    ;; Keep focus on the code buffer
    (select-window cur-window)))

(defun chicken--send-and-return (string)
  "Send STRING to CHICKEN REPL without echoing input."
  (unless (comint-check-proc "*chicken*")
    (set-buffer (make-comint "chicken" "/usr/bin/chicken-csi")))
  (let ((proc (get-buffer-process "*chicken*")))
    (comint-send-string proc (concat string "\n"))))

(defun chicken-send-region (start end)
  "Send the current region to the CHICKEN REPL."
  (interactive "r")
  (chicken--send-and-return (buffer-substring-no-properties start end)))

(defun chicken-send-buffer ()
  "Send the entire buffer to the CHICKEN REPL."
  (interactive)
  (chicken-send-region (point-min) (point-max)))

(defun chicken-send-definition ()
  "Send the current definition to the CHICKEN REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chicken-send-region (region-beginning) (region-end)))
  (deactivate-mark))

(defun chicken-clear-repl ()
  "Clear CHICKEN REPL buffer."
  (interactive)
  (with-current-buffer "*chicken*"
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

;; Keybindings for Scheme mode
(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") #'chicken-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-r") #'chicken-send-region)
  (define-key scheme-mode-map (kbd "C-c C-b") #'chicken-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-z") #'run-chicken)
  (define-key scheme-mode-map (kbd "C-c C-l") #'chicken-clear-repl))

(provide 'scheme-dev)
;;; scheme-dev.el ends here
