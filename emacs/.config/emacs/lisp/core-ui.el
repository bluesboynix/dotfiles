;;; core-ui.el --- Theme, font, modeline, and UI polish (GUI & terminal) -*- lexical-binding: t; -*-

;; ---------- Fonts ----------
(if (display-graphic-p)
    (progn
      (set-face-attribute 'default nil
                          :family "Fira Code"
                          :height 140
                          :weight 'normal)
      ;; Enable ligatures if fira-code-mode is available
      (when (require 'fira-code-mode nil t)
        (global-fira-code-mode 1)))
  ;; Terminal fallback
  (set-face-attribute 'default nil
                      :family "Monospace"
                      :height 120))

;; ---------- Doom Theme ----------
(require 'doom-themes)
;; Choose your favourite: doom-vibrant, doom-one, doom-peacock, doom-city-lights, etc.
(load-theme 'doom-vibrant t)

;; (Optional) Doom theme extras: flash outline, bold/italic tweaks
(doom-themes-visual-bell-config)      ; flash when you hit a wrong key
(doom-themes-neotree-config)          ; integrates with neotree if you use it
(doom-themes-treemacs-config)         ; if you ever use treemacs

;; ---------- Doom Modeline ----------
(require 'doom-modeline)
(setq doom-modeline-height 32
      doom-modeline-bar-width 3
      doom-modeline-icon (display-graphic-p)  ; icons only in GUI
      doom-modeline-major-mode-icon t
      doom-modeline-buffer-file-name-style 'truncate-upto-project)
(doom-modeline-mode 1)

;; ---------- Cursor ----------
(if (display-graphic-p)
    (setq-default cursor-type 'bar)
  (setq-default cursor-type 'underline))

;; ---------- UI Details ----------
;; Fringes (GUI only)
(when (display-graphic-p)
  (fringe-mode 10))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Better scrolling
(when (display-graphic-p)
  (setq scroll-step 1
        scroll-margin 3))

;; Messages limit
(setq message-log-max 1000)

(provide 'core-ui)
;;; core-ui.el ends here
