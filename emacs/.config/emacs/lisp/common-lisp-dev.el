;;; common-lisp-dev.el --- Common Lisp Development Config -*- lexical-binding: t; -*-

;; -----------------------------
;; Common Lisp (SLIME + SBCL)
;; -----------------------------
(use-package slime
  :ensure t
  :mode ("\\.lisp\\'" . lisp-mode)
  :commands (slime slime-connect)
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-complete-symbol*-fancy t)
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; -----------------------------
;; Smartparens
;; -----------------------------
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
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
  :hook ((slime-mode lisp-mode) . eldoc-box-hover-mode))

(add-hook 'slime-mode-hook #'eldoc-mode)
(add-hook 'lisp-mode-hook  #'eldoc-mode)

;; -----------------------------
;; Macro Expansion
;; -----------------------------
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; -----------------------------
;; Rainbow Delimiters (robust)
;; -----------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode
          lisp-mode
          emacs-lisp-mode
          sly-mrepl-mode
          slime-repl-mode)
         . rainbow-delimiters-mode)
  :config
  ;; Reapply colors after startup in case theme overwrites
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
;; Local Symbol Completion (via Corfu + built-ins)
;; -----------------------------
(defun my/lisp-symbol-completion-setup ()
  "Enable Lisp symbol completions via Corfu."
  (setq-local completion-at-point-functions
              (list #'lisp-complete-symbol)))

(add-hook 'lisp-mode-hook #'my/lisp-symbol-completion-setup)
(add-hook 'slime-repl-mode-hook #'my/lisp-symbol-completion-setup)

;; Optional: auto-popup completions
(with-eval-after-load 'corfu
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2))

(provide 'common-lisp-dev)
;;; common-lisp-dev.el ends here

;;   ;;; common-lisp-dev.el --- Common Lisp Development Config

;; ;; -----------------------------
;; ;; Common Lisp (SLIME + SBCL)
;; ;; -----------------------------
;; (use-package slime
;;   :ensure t
;;   :mode ("\\.lisp\\'" . lisp-mode)
;;   :commands (slime slime-connect)
;;   :init
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "sbcl")
;;   :config
;;   (slime-setup '(slime-fancy))
;;   (with-eval-after-load 'slime
;;     (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; ;; -----------------------------
;; ;; Smartparens
;; ;; -----------------------------
;; (use-package smartparens
;;   :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
;;   :config
;;   (require 'smartparens-config)
;;   (smartparens-global-mode 1)
;;   (show-smartparens-global-mode 1)
;;   (setq sp-autoescape-string-quote nil
;;         sp-show-pair-delay 0.2
;;         sp-highlight-pair-overlay nil))

;; ;; -----------------------------
;; ;; Eldoc and Hover Docs
;; ;; -----------------------------
;; (use-package eldoc-box
;;   :hook ((slime-mode lisp-mode) . eldoc-box-hover-mode))

;; (add-hook 'slime-mode-hook #'eldoc-mode)
;; (add-hook 'lisp-mode-hook  #'eldoc-mode)

;; ;; -----------------------------
;; ;; Macro Expansion
;; ;; -----------------------------
;; (use-package macrostep
;;   :bind (:map emacs-lisp-mode-map
;;               ("C-c e" . macrostep-expand)))

;; ;; -----------------------------
;; ;; Rainbow Delimiters (robust)
;; ;; -----------------------------
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook ((prog-mode
;;           lisp-mode
;;           emacs-lisp-mode
;;           sly-mrepl-mode
;;           slime-repl-mode)
;;          . rainbow-delimiters-mode)
;;   :config
;;   ;; Reapply colors after startup in case theme overwrites
;;   (add-hook 'emacs-startup-hook
;;             (lambda ()
;;               (with-eval-after-load 'rainbow-delimiters
;;                 (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FF0000")
;;                 (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FF8C00")
;;                 (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#FFFF00")
;;                 (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#00FF00")
;;                 (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#56B6C2")
;;                 (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#9467BD")
;;                 (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#D19A66")
;;                 (set-face-attribute 'rainbow-delimiters-unmatched-face nil
;;                                     :foreground "white" :background "#FF0000" :weight 'bold)))))

;; (provide 'common-lisp-dev)
;; ;;; cl-dev.el ends here
