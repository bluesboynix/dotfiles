;;; tool-rainbow-paren.el --- Rainbow parenthesis with soft colors -*- lexical-binding: t; -*-

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  ;; Start with Red (soft rose), then flow through the spectrum gently
  (rainbow-delimiters-depth-1-face ((t (:foreground "#e06c75"))))   ; Soft Red
  (rainbow-delimiters-depth-2-face ((t (:foreground "#d19a66"))))   ; Soft Orange
  (rainbow-delimiters-depth-3-face ((t (:foreground "#e5c07b"))))   ; Soft Yellow/Gold
  (rainbow-delimiters-depth-4-face ((t (:foreground "#98c379"))))   ; Soft Green
  (rainbow-delimiters-depth-5-face ((t (:foreground "#56b6c2"))))   ; Soft Cyan/Teal
  (rainbow-delimiters-depth-6-face ((t (:foreground "#61afef"))))   ; Soft Blue
  (rainbow-delimiters-depth-7-face ((t (:foreground "#c678dd"))))   ; Soft Purple
  :config
  ;; Cycle repeats after Purple -> back to Soft Red at depth 8
  (setq rainbow-delimiters-face-count 7))

(provide 'tool-rainbow-paren)
;;; tool-rainbow-paren.el ends here
