;;; tool-rainbow-paren.el --- Rainbow parenthesis with custom colors -*- lexical-binding: t; -*-

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "red"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "brown"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "pink"))))
  :config
  ;; Force the cycle to use exactly these 7 colors (repeats after pink)
  (setq rainbow-delimiters-face-count 7))

(provide 'tool-rainbow-paren)
;;; tool-rainbow-paren.el ends here
