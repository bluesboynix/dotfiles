;;; core-ui.el --- Theme, font, modeline, and UI polish (GUI & terminal) -*- lexical-binding: t; -*-

;; ---------- Fonts ----------
(if (display-graphic-p)
    (progn
      (set-face-attribute 'default nil
                          :font "FiraCode Nerd Font"
                          :height 120
                          :weight 'normal))
  ;; Terminal fallback
  (set-face-attribute 'default nil
                      :family "Monospace"
                      :height 120))

;; Built-in theme
(load-theme 'modus-vivendi t)

;; ---------- Cursor ----------
(if (display-graphic-p)
    (setq-default cursor-type 'bar)
  (setq-default cursor-type 'underline))

(blink-cursor-mode 1)
(setq blink-cursor-interval 0.6)
(setq blink-cursor-blinks 5)


(provide 'core-ui)
;;; core-ui.el ends here
