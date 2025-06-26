;; Lisp Development
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy)))

;; Smartparens configuration
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)  ; Enable globally
  (show-smartparens-global-mode 1)  ; Highlight matching pairs
  
  ;; Customize behavior
  (setq sp-autoescape-string-quote nil
        sp-show-pair-delay 0.2
        sp-highlight-pair-overlay nil)
  
  ;; SLIME-specific integration
  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "DEL") #'sp-backward-delete-char)))

;; eldoc
(use-package eldoc-box
  :hook (slime-mode . eldoc-box-hover-mode))

(add-hook 'slime-mode-hook #'eldoc-mode)

;; macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; rainbow coloring parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(require 'rainbow-delimiters)

;; (custom-set-faces
;;  ;; VIBGYOR order
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "#8A2BE2")))) ; Violet
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "#4B0082")))) ; Indigo
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#0000FF")))) ; Blue
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#00FF00")))) ; Green
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#FFFF00")))) ; Yellow
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#FFA500")))) ; Orange
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#FF0000")))) ; Red

;;  ;; Optional: unmatched parens
;;  '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "#FF0000" :weight bold)))))

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#FF0000")))) ; Red
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FF8C00")))) ; Orange
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFFF00")))) ; Yellow
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#00FF00")))) ; Green
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#56B6C2")))) ; Cyan
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#9467BD")))) ; Purple
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#D19A66")))) ; Bronze

 ;; Optional: unmatched delimiters (bright red background)
 '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "#FF0000" :weight bold)))))

(provide 'lisp-dev)
