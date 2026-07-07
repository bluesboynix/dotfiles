;;; tool-vterm.el --- Simple vterm toggles -*- lexical-binding: t; -*-

;; This file provides two commands to toggle a vterm in a side window:
;; - `tool-vterm-toggle-horizontal': bottom window, height 0.5 (half screen)
;; - `tool-vterm-toggle-vertical':   bottom window, height 0.25 (quarter screen)
;; Bindings: C-c t h and C-c t v.

(require 'vterm)

(defun tool-vterm--toggle (buffer-name side size)
  "Toggle display of BUFFER-NAME in a SIDE window of SIZE fraction.
SIDE: 'left, 'right, 'bottom, or 'top.
SIZE: 0..1 for width (left/right) or height (bottom/top)."
  (let* ((buf (get-buffer buffer-name))
         (win (and buf (get-buffer-window buf))))
    (if (and win (eq (window-parameter win 'window-side) side))
        ;; Close: delete the side window
        (delete-window win)
      ;; Open: create buffer if missing, then display in side window
      (unless buf
        (with-current-buffer (vterm buffer-name)))
      (display-buffer buffer-name
                      `((display-buffer-in-side-window)
                        (side . ,side)
                        (slot . 0)
                        (window-width  . ,(if (memq side '(left right)) size))
                        (window-height . ,(if (memq side '(top bottom)) size))
                        (preserve-size . (t . t)))))))

(defun tool-vterm-toggle-horizontal ()
  "Toggle horizontal vterm (bottom half)."
  (interactive)
  (tool-vterm--toggle "*vterm-bottom-half*" 'bottom 0.5))

(defun tool-vterm-toggle-vertical ()
  "Toggle vertical vterm (bottom quarter)."
  (interactive)
  (tool-vterm--toggle "*vterm-bottom-quarter*" 'bottom 0.25))

;; Keymap
;; (defvar tool-vterm-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "h" #'tool-vterm-toggle-horizontal)
;;     (define-key map "v" #'tool-vterm-toggle-vertical)
;;     map)
;;   "Keymap for tool-vterm commands.")

;;;###autoload
;;(define-key global-map (kbd "C-c t") tool-vterm-map)

(provide 'tool-vterm)
;;; tool-vterm.el ends here
