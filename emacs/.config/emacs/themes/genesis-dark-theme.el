;;; genesis-dark-theme.el --- Genesis Dark (Modus Vivendi style) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 2.0.0
;; Keywords: themes
;; URL: https://github.com/yourname/genesis-dark
;; Package-Requires: ((emacs "29.0"))

;;; Commentary:
;; Genesis Dark — pure black, high contrast, Modus Vivendi inspired.
;; A minimalist theme with pure RGB colors in the style of Modus Vivendi.

;;; Code:

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(deftheme genesis-dark
  "Genesis Dark — pure black, high contrast, Modus Vivendi style")

(let* ((class '((class color) (min-colors 89)))

       ;; =====================================================
       ;; Core Palette (Genesis original colors)
       ;; =====================================================
       
       ;; Base backgrounds (Modus: bg-main, bg-dim, bg-alt)
       (bg-main "#000000")        ; primary background
       (bg-dim "#111111")         ; dimmed background (like Modus bg-dim)
       (bg-alt "#222222")         ; alternative background
       
       ;; Base foregrounds (Modus: fg-main, fg-dim, fg-alt)
       (fg-main "#ffffff")        ; primary foreground
       (fg-dim "#b0b0b0")         ; dimmed foreground
       (fg-alt "#808080")         ; alternative foreground
       
       ;; Special purpose (Modus style)
       (bg-active "#404040")      ; active/selected background
       (bg-inactive "#202020")    ; inactive background
       (fg-active "#ffffff")      ; active foreground
       (fg-inactive "#707070")    ; inactive foreground
       
       ;; Pure RGB colors (Genesis original palette)
       (red "#ff0000")
       (green "#00ff00")
       (yellow "#ffff00")
       (blue "#0000ff")
       (magenta "#ffaaff")
       (cyan "#00ffff")
       
       ;; Extended colors (Modus naming conventions)
       (red-intense red)
       (green-intense green)
       (yellow-intense yellow)
       (blue-intense blue)
       (magenta-intense magenta)
       (cyan-intense cyan)
       
       (red-faint "#ff4444")      ; faint variants like Modus
       (green-faint "#44ff44")
       (yellow-faint "#ffff44")
       (blue-faint "#4444ff")
       (magenta-faint "#ffccff")
       (cyan-faint "#44ffff")
       
       (red-subtle "#330000")     ; subtle backgrounds
       (green-subtle "#003300")
       (yellow-subtle "#333300")
       (blue-subtle "#000033")
       (magenta-subtle "#330033")
       (cyan-subtle "#003333")
       
       ;; Special purpose colors (Modus style)
       (bg-completion "#222222")  ; completion background
       (bg-hover "#333333")       ; hover background
       (bg-hover-secondary "#2a2a2a")
       (bg-header "#111111")      ; header background
       (bg-header-alt "#222222")
       (bg-tab-bar "#111111")     ; tab bar background
       (bg-tab-active "#222222")
       (bg-tab-inactive "#1a1a1a")
       (bg-region "#404040")      ; region background
       (bg-hl-line "#111111")     ; highlight line
       (bg-paren-match "#404040") ; paren match
       
       ;; Border and separator colors (Modus style)
       (border "#333333")
       (border-dim "#222222")
       (border-subtle "#2a2a2a")
       (underline "#444444")
       
       ;; Modeline colors (Modus style)
       (bg-mode-line-active "#222222")
       (fg-mode-line-active "#ffffff")
       (bg-mode-line-inactive "#111111")
       (fg-mode-line-inactive "#808080")
       
       ;; Semantic colors (Modus naming)
       (fg-err red-intense)
       (fg-warning yellow-intense)
       (fg-note cyan-intense)
       (fg-success green-intense)
       
       ;; UI colors (Modus style)
       (bg-cursor "#ffffff")      ; cursor
       (fg-cursor "#000000")      ; cursor foreground
       (bg-mark "#333333")        ; marked region
       (bg-mark-other "#2a2a2a")  ; secondary marked
       
       ;; Common styles (like Modus)
       (bold '(:weight bold))
       (semibold '(:weight semibold))
       (light '(:weight light))
       (italic '(:slant italic))
       (underline-wave `(:underline (:style wave))))
  
  (custom-theme-set-faces
   'genesis-dark

   ;; =====================================================
   ;; Basic faces (Modus core faces)
   ;; =====================================================
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,bg-cursor :foreground ,fg-cursor))))
   `(region ((,class (:background ,bg-region))))
   `(secondary-selection ((,class (:background ,bg-hover))))
   `(fringe ((,class (:background ,bg-main))))
   `(hl-line ((,class (:background ,bg-hl-line))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border-dim))))
   `(window-divider-first-pixel ((,class (:foreground ,border-dim))))
   `(window-divider-last-pixel ((,class (:foreground ,border-dim))))
   `(shadow ((,class (:foreground ,fg-dim))))
   `(minibuffer-prompt ((,class (:foreground ,blue-intense ,@bold))))
   `(escape-glyph ((,class (:foreground ,magenta-intense))))
   `(homoglyph ((,class (:foreground ,yellow-intense))))
   `(nobreak-space ((,class (:underline ,underline))))
   `(nobreak-hyphen ((,class (:foreground ,red-intense))))
   `(help-argument-name ((,class (:foreground ,cyan-intense))))
   `(help-key-binding ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(trailing-whitespace ((,class (:background ,red-subtle))))
   `(fill-column-indicator ((,class (:foreground ,border))))
   `(header-line ((,class (:background ,bg-header :foreground ,fg-dim))))
   `(header-line-highlight ((,class (:background ,bg-header-alt))))
   `(tab-line ((,class (:background ,bg-tab-bar))))
   `(tab-bar ((,class (:background ,bg-tab-bar))))
   `(tab-bar-tab ((,class (:background ,bg-tab-active :foreground ,fg-main))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-tab-inactive :foreground ,fg-dim))))

   ;; =====================================================
   ;; Font lock faces (Modus style with Genesis colors)
   ;; =====================================================
   `(font-lock-builtin-face ((,class (:foreground ,magenta-intense))))
   `(font-lock-comment-face ((,class (:foreground ,fg-alt ,@italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim))))
   `(font-lock-constant-face ((,class (:foreground ,yellow-intense))))
   `(font-lock-doc-face ((,class (:foreground ,green-intense))))
   `(font-lock-doc-markup-face ((,class (:foreground ,magenta-intense))))
   `(font-lock-function-name-face ((,class (:foreground ,blue-intense))))
   `(font-lock-keyword-face ((,class (:foreground ,red-intense ,@bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,red-intense ,@bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta-intense))))
   `(font-lock-string-face ((,class (:foreground ,green-faint))))
   `(font-lock-type-face ((,class (:foreground ,cyan-intense))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-main))))
   `(font-lock-warning-face ((,class (:foreground ,yellow-intense ,@bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta-intense))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow-intense))))

   ;; Extended font lock (Emacs 29+)
   `(font-lock-number-face ((,class (:foreground ,yellow-intense))))
   `(font-lock-operator-face ((,class (:foreground ,magenta-intense))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg-dim))))
   `(font-lock-bracket-face ((,class (:foreground ,fg-main))))
   `(font-lock-escape-face ((,class (:foreground ,yellow-intense))))
   `(font-lock-property-use-face ((,class (:foreground ,cyan-intense))))
   `(font-lock-property-definition-face ((,class (:foreground ,blue-intense))))

   ;; =====================================================
   ;; Tree-sitter faces (Modus-like styling)
   ;; =====================================================
   `(treesit-font-lock-bracket-face ((,class (:foreground ,fg-dim))))
   `(treesit-font-lock-delimiter-face ((,class (:foreground ,fg-dim))))
   `(treesit-font-lock-punctuation-face ((,class (:foreground ,fg-dim))))
   `(treesit-font-lock-escape-face ((,class (:foreground ,yellow-intense))))
   `(treesit-font-lock-operator-face ((,class (:foreground ,magenta-intense))))
   `(treesit-font-lock-parameter-face ((,class (:foreground ,fg-main))))
   `(treesit-font-lock-property-face ((,class (:foreground ,cyan-intense))))
   `(treesit-font-lock-method-face ((,class (:foreground ,blue-intense))))
   `(treesit-font-lock-function-face ((,class (:foreground ,blue-intense))))
   `(treesit-font-lock-function-call-face ((,class (:foreground ,blue-intense))))
   `(treesit-font-lock-builtin-face ((,class (:foreground ,magenta-intense))))
   `(treesit-font-lock-type-face ((,class (:foreground ,cyan-intense))))
   `(treesit-font-lock-type-parameter-face ((,class (:foreground ,cyan-intense ,@italic))))
   `(treesit-font-lock-variable-face ((,class (:foreground ,fg-main))))
   `(treesit-font-lock-variable-reference-face ((,class (:foreground ,fg-main))))
   `(treesit-font-lock-variable-special-face ((,class (:foreground ,yellow-intense ,@bold))))
   `(treesit-font-lock-constant-face ((,class (:foreground ,yellow-intense))))
   `(treesit-font-lock-constant-builtin-face ((,class (:foreground ,yellow-intense ,@bold))))
   `(treesit-font-lock-number-face ((,class (:foreground ,yellow-intense))))
   `(treesit-font-lock-string-face ((,class (:foreground ,green-intense))))
   `(treesit-font-lock-doc-face ((,class (:foreground ,green-intense))))
   `(treesit-font-lock-comment-face ((,class (:foreground ,fg-alt ,@italic))))
   `(treesit-font-lock-keyword-face ((,class (:foreground ,red-intense ,@bold))))
   `(treesit-font-lock-attribute-face ((,class (:foreground ,cyan-intense))))
   `(treesit-font-lock-label-face ((,class (:foreground ,yellow-intense))))

   ;; =====================================================
   ;; Mode line (Modus style)
   ;; =====================================================
   `(mode-line ((,class (:background ,bg-mode-line-active
                         :foreground ,fg-mode-line-active
                         :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-mode-line-inactive
                                  :foreground ,fg-mode-line-inactive
                                  :box (:line-width 1 :color ,border-dim)))))
   `(mode-line-emphasis ((,class (:foreground ,blue-intense ,@bold))))
   `(mode-line-highlight ((,class (:background ,bg-active))))
   `(mode-line-buffer-id ((,class (:foreground ,fg-main ,@bold))))

   ;; =====================================================
   ;; Line numbers (Modus style)
   ;; =====================================================
   `(line-number ((,class (:background ,bg-main :foreground ,fg-dim))))
   `(line-number-current-line ((,class (:background ,bg-main :foreground ,fg-main ,@bold))))
   `(line-number-major-tick ((,class (:background ,bg-main :foreground ,fg-alt ,@bold))))
   `(line-number-minor-tick ((,class (:background ,bg-main :foreground ,fg-dim))))

   ;; =====================================================
   ;; Completion (Modus style)
   ;; =====================================================
   `(completions-common-part ((,class (:foreground ,yellow-intense))))
   `(completions-first-difference ((,class (:foreground ,red-intense))))
   `(completions-highlight ((,class (:background ,bg-hover))))
   `(completion-preview-face ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(completion-preview-exact-face ((,class (:background ,bg-active :foreground ,fg-main))))
   
   ;; Vertico (popular completion UI)
   `(vertico-current ((,class (:background ,bg-active :foreground ,fg-main ,@bold))))
   `(vertico-group-title ((,class (:foreground ,fg-alt ,@bold))))
   `(vertico-group-separator ((,class (:foreground ,border))))
   `(vertico-multiline ((,class (:background ,bg-dim))))
   
   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,blue-intense ,@bold))))
   `(orderless-match-face-1 ((,class (:foreground ,magenta-intense ,@bold))))
   `(orderless-match-face-2 ((,class (:foreground ,cyan-intense ,@bold))))
   `(orderless-match-face-3 ((,class (:foreground ,yellow-intense ,@bold))))

   ;; =====================================================
   ;; Search and replace (Modus style)
   ;; =====================================================
   `(isearch ((,class (:background ,yellow-intense :foreground ,bg-main ,@bold))))
   `(isearch-fail ((,class (:background ,red-subtle :foreground ,fg-err))))
   `(lazy-highlight ((,class (:background ,cyan-subtle :foreground ,fg-main))))
   `(match ((,class (:background ,yellow-subtle :foreground ,fg-main))))
   `(query-replace ((,class (:background ,magenta-subtle :foreground ,fg-main))))

   ;; =====================================================
   ;; Buttons and links (Modus style)
   ;; =====================================================
   `(button ((,class (:foreground ,blue-intense :underline t))))
   `(link ((,class (:foreground ,blue-intense :underline t))))
   `(link-visited ((,class (:foreground ,magenta-intense :underline t))))
   `(tooltip ((,class (:background ,bg-alt :foreground ,fg-main
                       :box (:line-width 1 :color ,border)))))

   ;; =====================================================
   ;; Parentheses matching (Modus style)
   ;; =====================================================
   `(show-paren-match ((,class (:background ,bg-paren-match :foreground ,fg-main ,@bold))))
   `(show-paren-match-expression ((,class (:background ,bg-hover))))
   `(show-paren-mismatch ((,class (:background ,red-intense :foreground ,bg-main))))

   ;; =====================================================
   ;; Diff and VC (Modus style)
   ;; =====================================================
   `(diff-added ((,class (:background ,green-subtle :foreground ,green-intense))))
   `(diff-changed ((,class (:background ,yellow-subtle :foreground ,yellow-intense))))
   `(diff-removed ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(diff-refine-added ((,class (:background ,green-intense :foreground ,bg-main))))
   `(diff-refine-changed ((,class (:background ,yellow-intense :foreground ,bg-main))))
   `(diff-refine-removed ((,class (:background ,red-intense :foreground ,bg-main))))
   `(diff-context ((,class (:foreground ,fg-dim))))
   `(diff-file-header ((,class (:background ,bg-alt :foreground ,fg-main ,@bold))))
   `(diff-hunk-header ((,class (:background ,bg-dim :foreground ,fg-alt))))
   `(diff-header ((,class (:background ,bg-dim :foreground ,fg-dim))))

   ;; =====================================================
   ;; Diagnostics and flymake (Modus style)
   ;; =====================================================
   `(flymake-error ((,class ,@underline-wave :foreground ,red-intense)))
   `(flymake-warning ((,class ,@underline-wave :foreground ,yellow-intense)))
   `(flymake-note ((,class ,@underline-wave :foreground ,cyan-intense)))
   `(flymake-error-at-eol ((,class (:background ,red-subtle :foreground ,fg-err))))
   
   `(flycheck-error ((,class ,@underline-wave :foreground ,red-intense)))
   `(flycheck-warning ((,class ,@underline-wave :foreground ,yellow-intense)))
   `(flycheck-info ((,class ,@underline-wave :foreground ,cyan-intense)))
   `(flycheck-fringe-error ((,class (:foreground ,red-intense))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow-intense))))
   `(flycheck-fringe-info ((,class (:foreground ,cyan-intense))))

   ;; =====================================================
   ;; Org mode (Modus style with Genesis colors)
   ;; =====================================================
   `(org-level-1 ((,class (:foreground ,red-intense ,@bold :height 1.2))))
   `(org-level-2 ((,class (:foreground ,cyan-intense ,@bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,blue-intense ,@bold))))
   `(org-level-4 ((,class (:foreground ,yellow-intense ,@bold))))
   `(org-level-5 ((,class (:foreground ,magenta-intense ,@bold))))
   `(org-level-6 ((,class (:foreground ,green-intense ,@bold))))
   `(org-level-7 ((,class (:foreground ,cyan-faint ,@bold))))
   `(org-level-8 ((,class (:foreground ,blue-faint ,@bold))))
   
   `(org-todo ((,class (:foreground ,red-intense ,@bold))))
   `(org-done ((,class (:foreground ,green-intense ,@bold))))
   `(org-date ((,class (:foreground ,cyan-intense))))
   `(org-link ((,class (:foreground ,blue-intense :underline t))))
   `(org-code ((,class (:foreground ,yellow-intense))))
   `(org-verbatim ((,class (:foreground ,green-intense))))
   `(org-quote ((,class (:background ,bg-dim :foreground ,fg-dim ,@italic))))
   `(org-special-keyword ((,class (:foreground ,fg-alt))))
   `(org-document-info ((,class (:foreground ,fg-alt))))
   `(org-document-title ((,class (:foreground ,fg-main ,@bold :height 1.3))))
   `(org-document-info-keyword ((,class (:foreground ,fg-dim))))
   `(org-block ((,class (:background ,bg-dim))))
   `(org-block-begin-line ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(org-block-end-line ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(org-meta-line ((,class (:foreground ,fg-dim))))
   `(org-checkbox ((,class (:foreground ,cyan-intense))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,red-intense))))
   `(org-checkbox-statistics-done ((,class (:foreground ,green-intense))))
   `(org-agenda-structure ((,class (:foreground ,cyan-intense ,@bold))))
   `(org-agenda-date ((,class (:foreground ,blue-intense))))
   `(org-agenda-date-today ((,class (:foreground ,blue-intense ,@bold))))
   `(org-agenda-date-weekend ((,class (:foreground ,magenta-intense))))
   `(org-agenda-current-time ((,class (:foreground ,yellow-intense))))
   `(org-agenda-done ((,class (:foreground ,fg-dim))))
   `(org-scheduled ((,class (:foreground ,fg-main))))
   `(org-scheduled-today ((,class (:foreground ,yellow-intense))))
   `(org-scheduled-previously ((,class (:foreground ,red-intense))))
   `(org-time-grid ((,class (:foreground ,fg-dim))))
   `(org-upcoming-deadline ((,class (:foreground ,red-intense))))
   `(org-upcoming-distant-deadline ((,class (:foreground ,yellow-intense))))
   `(org-priority ((,class (:foreground ,magenta-intense))))

   ;; =====================================================
   ;; Magit (Modus style)
   ;; =====================================================
   `(magit-section-heading ((,class (:foreground ,blue-intense ,@bold))))
   `(magit-section-highlight ((,class (:background ,bg-dim))))
   `(magit-branch-local ((,class (:foreground ,cyan-intense))))
   `(magit-branch-remote ((,class (:foreground ,green-intense))))
   `(magit-hash ((,class (:foreground ,fg-dim))))
   `(magit-tag ((,class (:foreground ,yellow-intense))))
   `(magit-signature-good ((,class (:foreground ,green-intense))))
   `(magit-signature-bad ((,class (:foreground ,red-intense))))
   `(magit-diff-added ((,class (:background ,green-subtle :foreground ,green-intense))))
   `(magit-diff-added-highlight ((,class (:background ,green-subtle :foreground ,green-intense))))
   `(magit-diff-removed ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(magit-diff-removed-highlight ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(magit-diff-context ((,class (:foreground ,fg-dim))))
   `(magit-diff-context-highlight ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-active :foreground ,fg-main))))
   `(magit-diff-file-heading ((,class (:foreground ,fg-main ,@bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-dim :foreground ,fg-main ,@bold))))

   ;; =====================================================
   ;; Dired (Modus style)
   ;; =====================================================
   `(dired-directory ((,class (:foreground ,blue-intense ,@bold))))
   `(dired-flagged ((,class (:foreground ,red-intense ,@bold))))
   `(dired-marked ((,class (:foreground ,yellow-intense ,@bold))))
   `(dired-symlink ((,class (:foreground ,magenta-intense))))
   `(dired-broken-symlink ((,class (:foreground ,red-intense))))
   `(dired-header ((,class (:foreground ,cyan-intense ,@bold))))
   `(dired-ignored ((,class (:foreground ,fg-dim))))

   ;; =====================================================
   ;; Company mode (Modus style)
   ;; =====================================================
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(company-tooltip-selection ((,class (:background ,bg-active :foreground ,fg-main ,@bold))))
   `(company-tooltip-common ((,class (:foreground ,yellow-intense ,@bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,yellow-intense ,@bold))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-dim))))
   `(company-tooltip-scrollbar-thumb ((,class (:background ,border))))
   `(company-tooltip-scrollbar-track ((,class (:background ,bg-dim))))
   `(company-preview ((,class (:background ,bg-dim))))
   `(company-preview-common ((,class (:foreground ,yellow-intense))))
   `(company-scrollbar-bg ((,class (:background ,bg-dim))))
   `(company-scrollbar-fg ((,class (:background ,border))))

   ;; =====================================================
   ;; Which-key (Modus style)
   ;; =====================================================
   `(which-key-key-face ((,class (:foreground ,blue-intense ,@bold))))
   `(which-key-group-description-face ((,class (:foreground ,magenta-intense))))
   `(which-key-command-description-face ((,class (:foreground ,fg-main))))
   `(which-key-local-map-description-face ((,class (:foreground ,cyan-intense))))
   `(which-key-separator-face ((,class (:foreground ,fg-dim))))

   ;; =====================================================
   ;; Whitespace (Modus style)
   ;; =====================================================
   `(whitespace-space ((,class (:foreground ,bg-dim :background ,bg-main))))
   `(whitespace-tab ((,class (:foreground ,border :background ,bg-dim))))
   `(whitespace-newline ((,class (:foreground ,border))))
   `(whitespace-trailing ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(whitespace-line ((,class (:background ,bg-dim))))
   `(whitespace-space-before-tab ((,class (:background ,yellow-subtle))))
   `(whitespace-indentation ((,class (:foreground ,border))))
   `(whitespace-empty ((,class (:background ,bg-dim))))
   `(whitespace-space-after-tab ((,class (:background ,yellow-subtle))))

   ;; =====================================================
   ;; Markdown (Modus style)
   ;; =====================================================
   `(markdown-header-face-1 ((,class (:foreground ,red-intense ,@bold :height 1.2))))
   `(markdown-header-face-2 ((,class (:foreground ,cyan-intense ,@bold :height 1.1))))
   `(markdown-header-face-3 ((,class (:foreground ,blue-intense ,@bold))))
   `(markdown-code-face ((,class (:background ,bg-dim))))
   `(markdown-inline-code-face ((,class (:foreground ,yellow-intense))))
   `(markdown-link-face ((,class (:foreground ,blue-intense))))
   `(markdown-url-face ((,class (:foreground ,cyan-intense))))
   `(markdown-markup-face ((,class (:foreground ,fg-dim))))
   `(markdown-list-face ((,class (:foreground ,cyan-intense))))
   `(markdown-blockquote-face ((,class (:background ,bg-dim :foreground ,fg-dim ,@italic))))
   `(markdown-pre-face ((,class (:background ,bg-dim))))
   `(markdown-table-face ((,class (:foreground ,fg-main))))

   ;; =====================================================
   ;; LSP (Modus style)
   ;; =====================================================
   `(lsp-face-semhl-variable ((,class (:foreground ,fg-main))))
   `(lsp-face-semhl-parameter ((,class (:foreground ,fg-main))))
   `(lsp-face-semhl-function ((,class (:foreground ,blue-intense))))
   `(lsp-face-semhl-method ((,class (:foreground ,blue-intense))))
   `(lsp-face-semhl-class ((,class (:foreground ,cyan-intense))))
   `(lsp-face-semhl-keyword ((,class (:foreground ,red-intense))))
   `(lsp-face-highlight-textual ((,class (:background ,bg-hl-line))))
   `(lsp-face-highlight-read ((,class (:background ,bg-hover))))
   `(lsp-face-highlight-write ((,class (:background ,bg-active))))
   `(lsp-face-semhl-type ((,class (:foreground ,cyan-intense))))
   `(lsp-face-semhl-enum ((,class (:foreground ,cyan-intense))))
   `(lsp-face-semhl-namespace ((,class (:foreground ,magenta-intense))))

   ;; =====================================================
   ;; Rainbow delimiters (Modus style)
   ;; =====================================================
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,red-intense))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow-intense))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,green-intense))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,cyan-intense))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue-intense))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,magenta-intense))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,red-faint))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow-faint))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green-faint))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red-intense ,@bold))))

   ;; =====================================================
   ;; Customize interface (Modus style)
   ;; =====================================================
   `(custom-group-tag ((,class (:foreground ,blue-intense ,@bold :height 1.2))))
   `(custom-variable-tag ((,class (:foreground ,cyan-intense ,@bold))))
   `(custom-face-tag ((,class (:foreground ,magenta-intense ,@bold))))
   `(custom-state ((,class (:foreground ,green-intense))))
   `(custom-button ((,class (:background ,bg-dim :foreground ,fg-main
                             :box (:line-width 1 :color ,border)))))
   `(custom-button-mouse ((,class (:background ,bg-active :foreground ,fg-main
                                   :box (:line-width 1 :color ,border)))))
   `(custom-button-pressed ((,class (:background ,bg-active :foreground ,fg-main
                                     :box (:line-width 1 :color ,border :style pressed-button)))))
   `(custom-button-pressed-unraised ((,class (:foreground ,red-intense :underline t))))
   `(custom-button-unraised ((,class (:underline t))))
   `(custom-changed ((,class (:background ,blue-subtle :foreground ,blue-intense))))
   `(custom-comment ((,class (:background ,bg-dim))))
   `(custom-comment-tag ((,class (:foreground ,fg-dim))))
   `(custom-documentation ((,class (:foreground ,fg-main))))
   `(custom-group-subtitle ((,class (:foreground ,fg-main ,@bold))))
   `(custom-invalid ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(custom-link ((,class (:foreground ,blue-intense :underline t))))
   `(custom-modified ((,class (:background ,blue-subtle :foreground ,blue-intense))))
   `(custom-rogue ((,class (:background ,red-subtle :foreground ,red-intense))))
   `(custom-saved ((,class (:foreground ,green-intense :underline t))))
   `(custom-set ((,class (:background ,yellow-subtle :foreground ,yellow-intense))))
   `(custom-themed ((,class (:background ,blue-subtle :foreground ,blue-intense))))
   `(custom-visibility ((,class (:foreground ,cyan-intense :underline t))))

   ;; =====================================================
   ;; Message and mail (Modus style)
   ;; =====================================================
   `(message-header-to ((,class (:foreground ,blue-intense ,@bold))))
   `(message-header-cc ((,class (:foreground ,cyan-intense))))
   `(message-header-subject ((,class (:foreground ,yellow-intense ,@bold))))
   `(message-header-newsgroups ((,class (:foreground ,magenta-intense))))
   `(message-header-other ((,class (:foreground ,red-intense))))
   `(message-header-name ((,class (:foreground ,cyan-intense))))
   `(message-header-xheader ((,class (:foreground ,fg-dim))))
   `(message-separator ((,class (:foreground ,fg-dim))))
   `(message-cited-text ((,class (:foreground ,fg-dim))))
   `(message-mml ((,class (:foreground ,green-intense))))

   ;; =====================================================
   ;; Terminal colors
   ;; =====================================================
   `(term-color-black ((,class (:foreground ,bg-main :background ,bg-main))))
   `(term-color-red ((,class (:foreground ,red-intense :background ,red-intense))))
   `(term-color-green ((,class (:foreground ,green-intense :background ,green-intense))))
   `(term-color-yellow ((,class (:foreground ,yellow-intense :background ,yellow-intense))))
   `(term-color-blue ((,class (:foreground ,blue-intense :background ,blue-intense))))
   `(term-color-magenta ((,class (:foreground ,magenta-intense :background ,magenta-intense))))
   `(term-color-cyan ((,class (:foreground ,cyan-intense :background ,cyan-intense))))
   `(term-color-white ((,class (:foreground ,fg-main :background ,fg-main))))

   ;; =====================================================
   ;; Eglot
   ;; =====================================================
   `(eglot-highlight-symbol-face ((,class (:background ,bg-hover ,@bold))))

   ;; =====================================================
   ;; Ivy/Selectrum/Swiper
   ;; =====================================================
   `(ivy-current-match ((,class (:background ,bg-active :foreground ,fg-main ,@bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,fg-dim))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow-intense ,@bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,cyan-intense ,@bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,magenta-intense ,@bold))))
   `(ivy-virtual ((,class (:foreground ,fg-dim))))
   `(ivy-subdir ((,class (:foreground ,blue-intense))))

   `(swiper-line-face ((,class (:background ,bg-hover))))
   `(swiper-match-face-1 ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(swiper-match-face-2 ((,class (:background ,yellow-subtle :foreground ,yellow-intense))))
   `(swiper-match-face-3 ((,class (:background ,cyan-subtle :foreground ,cyan-intense))))
   `(swiper-match-face-4 ((,class (:background ,magenta-subtle :foreground ,magenta-intense))))

   ;; =====================================================
   ;; Hydra
   ;; =====================================================
   `(hydra-face-red ((,class (:foreground ,red-intense ,@bold))))
   `(hydra-face-blue ((,class (:foreground ,blue-intense ,@bold))))
   `(hydra-face-amaranth ((,class (:foreground ,magenta-intense ,@bold))))
   `(hydra-face-teal ((,class (:foreground ,cyan-intense ,@bold))))

   ;; =====================================================
   ;; Avy
   ;; =====================================================
   `(avy-background-face ((,class (:foreground ,fg-dim))))
   `(avy-lead-face ((,class (:background ,yellow-intense :foreground ,bg-main ,@bold))))
   `(avy-lead-face-0 ((,class (:background ,cyan-intense :foreground ,bg-main ,@bold))))
   `(avy-lead-face-1 ((,class (:background ,magenta-intense :foreground ,bg-main ,@bold))))
   `(avy-lead-face-2 ((,class (:background ,green-intense :foreground ,bg-main ,@bold))))

   ;; =====================================================
   ;; Elfeed
   ;; =====================================================
   `(elfeed-search-title-face ((,class (:foreground ,fg-main))))
   `(elfeed-search-unread-title-face ((,class (:foreground ,blue-intense ,@bold))))
   `(elfeed-search-date-face ((,class (:foreground ,fg-dim))))
   `(elfeed-search-feed-face ((,class (:foreground ,cyan-intense))))
   `(elfeed-search-tag-face ((,class (:foreground ,yellow-intense))))
   `(elfeed-search-unread-count-face ((,class (:foreground ,fg-main))))
   `(elfeed-search-filter-face ((,class (:foreground ,blue-intense ,@bold))))

   ;; =====================================================
   ;; Indent guides
   ;; =====================================================
   `(highlight-indent-guides-character-face ((,class (:foreground ,border))))
   `(highlight-indent-guides-stack-odd-face ((,class (:foreground ,border))))
   `(highlight-indent-guides-stack-even-face ((,class (:foreground ,border-dim))))
   `(highlight-indent-guides-top-odd-face ((,class (:foreground ,fg-dim))))
   `(highlight-indent-guides-top-even-face ((,class (:foreground ,fg-dim))))

   ;; =====================================================
   ;; Anzu
   ;; =====================================================
   `(anzu-mode-line ((,class (:foreground ,yellow-intense ,@bold))))
   `(anzu-match-1 ((,class (:background ,cyan-subtle :foreground ,cyan-intense))))
   `(anzu-match-2 ((,class (:background ,magenta-subtle :foreground ,magenta-intense))))
   `(anzu-match-3 ((,class (:background ,green-subtle :foreground ,green-intense))))
   `(anzu-replace-highlight ((,class (:background ,bg-active :foreground ,fg-main))))
   `(anzu-replace-to ((,class (:background ,yellow-subtle :foreground ,yellow-intense))))

   ;; =====================================================
   ;; Undo-tree
   ;; =====================================================
   `(undo-tree-visualizer-current-face ((,class (:foreground ,yellow-intense ,@bold))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,fg-main))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,cyan-intense))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,magenta-intense))))

   ;; =====================================================
   ;; Solaire mode
   ;; =====================================================
   `(solaire-mode-line-face ((,class (:inherit mode-line))))
   `(solaire-mode-line-inactive-face ((,class (:inherit mode-line-inactive))))
   `(solaire-default-face ((,class (:background ,bg-dim))))
   `(solaire-hl-line-face ((,class (:background ,bg-hl-line))))

   ;; =====================================================
   ;; Tab bar
   ;; =====================================================
   `(tab-bar ((,class (:background ,bg-tab-bar))))
   `(tab-bar-tab ((,class (:background ,bg-tab-active :foreground ,fg-main))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-tab-inactive :foreground ,fg-dim))))

   ;; =====================================================
   ;; Centaur tabs
   ;; =====================================================
   `(centaur-tabs-active-bar-face ((,class (:background ,blue-intense))))
   `(centaur-tabs-selected ((,class (:background ,bg-main :foreground ,fg-main ,@bold))))
   `(centaur-tabs-selected-modified ((,class (:background ,bg-main :foreground ,fg-main ,@bold))))
   `(centaur-tabs-unselected ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(centaur-tabs-unselected-modified ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(centaur-tabs-close-mouse-face ((,class (:foreground ,red-intense))))
   `(centaur-tabs-default ((,class (:background ,bg-main :foreground ,fg-main))))

   ;; =====================================================
   ;; End of faces
   ;; =====================================================
   ))

(provide-theme 'genesis-dark)

;;; genesis-dark-theme.el ends here
