;; ====================
;; Keybindings & Leader Key
;; ====================
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for custom leader commands.")
(global-set-key (kbd "C-c m") my-leader-map)

;; Example bindings
(define-key my-leader-map (kbd "t") #'treemacs)  ; Leader + t for treemacs
(global-set-key (kbd "<f8>") #'treemacs)         ; F8 for treemacs
(global-set-key (kbd "<f9>") #'vterm-toggle)     ; F9 for vterm

(provide 'keybindings)
