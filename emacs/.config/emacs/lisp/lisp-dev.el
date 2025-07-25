;; Lisp Development

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(use-package slime
  :ensure t
  :init
  (load (expand-file-name "sbcl"))
  :config
  (slime-setup '(slime-fancy)))

;; Smartparens configuration
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil)

  ;; SLIME-specific integration
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; Eldoc and eldoc-box for hover tooltips
(use-package eldoc-box
  :hook ((slime-mode lisp-mode) . eldoc-box-hover-mode))

(add-hook 'slime-mode-hook #'eldoc-mode)
(add-hook 'lisp-mode-hook  #'eldoc-mode)

;; macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; rainbow-delimiters for colorful parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(require 'rainbow-delimiters)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#FF0000")))) ; Red
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FF8C00")))) ; Orange
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFFF00")))) ; Yellow
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#00FF00")))) ; Green
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#56B6C2")))) ; Cyan
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#9467BD")))) ; Purple
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#D19A66")))) ; Bronze
 '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "#FF0000" :weight bold)))))

(provide 'lisp-dev)

