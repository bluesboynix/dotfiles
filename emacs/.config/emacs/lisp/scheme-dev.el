;;; scheme-dev.el --- Scheme Development Config

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
          scheme-mode
          scheme-ts-mode
          sly-mrepl-mode
          slime-repl-mode)
         . rainbow-delimiters-mode)
  :config
  ;; Explicitly enable for scheme hooks
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-ts-mode-hook #'rainbow-delimiters-mode)
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

;; Enable rainbow in Gambit REPL buffers
(add-hook 'comint-mode-hook
          (lambda ()
            (when (string-match-p "\\*gambit\\*" (buffer-name))
              (rainbow-delimiters-mode 1))))

;; -----------------------------
;; Gambit Scheme (Comint Mode)
;; -----------------------------
(defun run-gambit ()
  "Run Gambit Scheme REPL in a comint buffer."
  (interactive)
  (unless (comint-check-proc "*gambit*")
    (set-buffer (make-comint "gambit" "gsi")))
  (pop-to-buffer-same-window "*gambit*"))

(defun gambit--send-and-return (string)
  "Send STRING to Gambit REPL without echoing the input."
  (unless (comint-check-proc "*gambit*")
    (set-buffer (make-comint "gambit" "gsi")))
  (let ((proc (get-buffer-process "*gambit*")))
    (comint-send-string proc (concat string "\n"))))

(defun gambit-send-region (start end)
  "Send the current region to the Gambit REPL."
  (interactive "r")
  (gambit--send-and-return (buffer-substring-no-properties start end)))

(defun gambit-send-buffer ()
  "Send the whole buffer to the Gambit REPL."
  (interactive)
  (gambit-send-region (point-min) (point-max)))

(defun gambit-send-definition ()
  "Send the current definition to the Gambit REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (gambit-send-region (region-beginning) (region-end)))
  (deactivate-mark))

(defun gambit-clear-repl ()
  "Clear Gambit REPL buffer."
  (interactive)
  (with-current-buffer "*gambit*"
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

;; Keybindings for Scheme mode
(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") #'gambit-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-r") #'gambit-send-region)
  (define-key scheme-mode-map (kbd "C-c C-b") #'gambit-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-z") #'run-gambit))



;; -------------------------------------------------
;; Tree-sitter quoted-data highlighting (Scheme)
;; -------------------------------------------------
;; Requires Emacs 29+ and `M-x treesit-install-language-grammar RET scheme RET`
;; to install the Scheme grammar.

;; Face for everything inside a quoted form:  '( ... )
(defface my-scheme-quoted-face
  '((t :slant italic :foreground "MediumSpringGreen"))
  "Face for quoted S-expressions in scheme-ts-mode.")

(defun my-scheme-ts-quoted-highlights ()
  "Tree-sitter patterns to highlight quoted data."
  `((quoted (datum) @my-scheme-quoted-face)))

(with-eval-after-load 'scheme-ts-mode
  (tree-sitter-hl-add-patterns 'scheme
    (my-scheme-ts-quoted-highlights)))

(provide 'scheme-dev)
;;; scheme-dev.el ends here
