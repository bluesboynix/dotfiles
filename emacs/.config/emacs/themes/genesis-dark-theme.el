;;; genesis-dark-theme.el --- A high-contrast pure black theme for Emacs
;;;
;;; Commentary:
;;; High-contrast dark theme with solid black background and vivid, unaltered colors.
;;; No font weights or slants for clarity and uniformity.
;;;
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(deftheme genesis-dark "Genesis High Contrast — solid black theme")

(let* ((class '((class color) (min-colors 89)))
       ;; High-contrast palette with pure colors
       (bg         "#000000")
       (fg         "#ffffff")
       (muted      "#b0b0b0")
       (comment    "#808080")
       (cursor     "#ffffff")
       (blue       "#0000ff")
       (light-blue "#afdfff")
       (green      "#00ff00")
       (midgreen  "#55cc55")
       (red        "#ff0000")
       (yellow     "#ffff00")
       (cyan       "#00ffff")
       (magenta    "#ffaaff")
       (selection  "#404040")
       (line       "#101010")
       (modeline-bg "#222222")
       (modeline-fg "#ffffff"))

  (custom-theme-set-faces
   'genesis-dark

   ;; Basics
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:background ,line))))
   `(fringe ((,class (:background ,bg))))
   `(shadow ((,class (:foreground ,muted))))
   `(minibuffer-prompt ((,class (:foreground ,light-blue))))

   ;; Font-lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,magenta))))
   `(font-lock-constant-face ((,class (:foreground ,yellow))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,light-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,red))))
   `(font-lock-string-face ((,class (:foreground ,midgreen))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground ,yellow))))

   ;; Mode line
   `(mode-line ((,class (:background ,modeline-bg :foreground ,modeline-fg :box nil))))
   `(mode-line-inactive ((,class (:background ,line :foreground ,muted :box nil))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,comment :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,bg))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,light-blue))))
   `(org-level-2 ((,class (:foreground ,green))))
   `(org-level-3 ((,class (:foreground ,cyan))))
   `(org-todo ((,class (:foreground ,red))))
   `(org-done ((,class (:foreground ,green))))
   `(org-date ((,class (:foreground ,yellow))))
   `(org-link ((,class (:foreground ,blue :underline t))))

   ;; UI elements
   `(link ((,class (:foreground ,blue :underline t))))
   `(button ((,class (:foreground ,blue))))
   `(tooltip ((,class (:background ,line :foreground ,fg))))

   ;; Search
   `(isearch ((,class (:background ,yellow :foreground ,bg))))
   `(lazy-highlight ((,class (:background ,cyan :foreground ,bg))))

   ;; Diff
   `(diff-added ((,class (:foreground ,green))))
   `(diff-removed ((,class (:foreground ,red))))
   `(diff-changed ((,class (:foreground ,yellow))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))

   ;; Company
   `(company-tooltip ((,class (:background ,line :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,selection :foreground ,fg))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))

   ;; Parenthesis matching
   `(show-paren-match ((,class (:background ,line :foreground ,fg))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,bg))))

   ;; Header-line
   `(header-line ((,class (:background ,bg :foreground ,muted))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,blue))))

   ) ; end faces

  (provide-theme 'genesis-dark))

;;; genesis-dark-theme.el ends here
