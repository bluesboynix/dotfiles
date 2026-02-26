;;; ui-rainbow-delimiters.el --- Rainbow delimiters -*- lexical-binding: t; -*-

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode))
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FF0000")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FF8C00")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#FFFF00")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#00FF00")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#56B6C2")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#9467BD")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#D19A66")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "white" :background "#FF0000" :weight 'bold))

(provide 'ui-rainbow-delimiters)
;;; ui-rainbow-delimiters.el ends here
