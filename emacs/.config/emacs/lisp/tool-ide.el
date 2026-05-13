;;; tool-ide.el --- Tree-sitter + Eglot IDE configuration -*- lexical-binding: t; -*-

;; ===================================================================
;; 1. Tree-sitter grammar installation helper
;; ===================================================================
(defun my/ensure-treesit-grammar (lang)
  "Install tree-sitter grammar for LANG if not already present.
LANG is a symbol like 'python, 'javascript, 'go, etc."
  (unless (treesit-language-available-p lang)
    (when (y-or-n-p (format "Tree-sitter grammar for %s not found. Install now? " lang))
      (treesit-install-language-grammar lang))))

;; Optional: preload common grammars (uncomment after first run)
;; (dolist (lang '(python javascript typescript go rust c cpp css html json))
;;   (my/ensure-treesit-grammar lang))

;; ===================================================================
;; 2. Configure major modes to use tree-sitter variants
;; ===================================================================

;; ----- Frontend -----
;; TypeScript / JavaScript (with tsx/jsx)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)))

;; JSON
(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'" . json-ts-mode))

;; HTML, CSS, SCSS
(use-package html-ts-mode
  :ensure nil
  :mode ("\\.html?\\'" . html-ts-mode))

(use-package css-ts-mode
  :ensure nil
  :mode ("\\.css\\'" . css-ts-mode))

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode))   ; no built-in tree-sitter for SCSS yet

;; Vue / Svelte (no built-in -ts-mode, use plain mode + LSP)
(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode))

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))

;; ----- Backend -----
;; Python
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode))

;; Go
(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode))

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode))

;; Java
(use-package java-ts-mode
  :ensure nil
  :mode ("\\.java\\'" . java-ts-mode))

;; C / C++
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)))

;; C# (use csharp-mode, no built-in -ts-mode yet)
(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'" . csharp-mode))

;; Ruby (no built-in -ts-mode, but ruby-ts-mode exists in Emacs 30?)
;; Actually Emacs 30 has ruby-ts-mode as built-in.
(use-package ruby-ts-mode
  :ensure nil
  :mode ("\\.rb\\'" . ruby-ts-mode))

;; PHP (no built-in -ts-mode, use php-mode)
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

;; ----- Scripting / Others -----
;; Bash
(use-package bash-ts-mode
  :ensure nil
  :mode ("\\.sh\\'" . bash-ts-mode))

;; Lua (no built-in -ts-mode, use lua-mode)
(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode))

;; Perl (use cperl-mode)
(use-package cperl-mode
  :ensure nil
  :mode ("\\.pl\\'" . cperl-mode))

;; R (use ess)
(use-package ess
  :ensure t
  :mode ("\\.R\\'" . R-mode))

;; Elixir (elixir-mode, no built-in -ts-mode)
(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" . elixir-mode)
  :mode ("\\.exs\\'" . elixir-mode))

;; ===================================================================
;; 3. Eglot configuration (auto-start + keybindings)
;; ===================================================================
(use-package eglot
  :ensure nil
  :hook ((python-ts-mode
          go-ts-mode
          rust-ts-mode
          java-ts-mode
          c-ts-mode
          c++-ts-mode
          ruby-ts-mode
          js-ts-mode
          typescript-ts-mode
          tsx-ts-mode
          html-ts-mode
          css-ts-mode
          json-ts-mode
          bash-ts-mode
         ;; non-ts modes:
          scss-mode
          vue-mode
          svelte-mode
          csharp-mode
          php-mode
          lua-mode
          cperl-mode
          R-mode
          elixir-mode)
         . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-extend-to-xref t)   ; also provide completions for xref results
  
  ;; Optional: suppress eglot's own completion UI, let Corfu handle it
  (setq eglot--managed-mode-hook
        (lambda ()
          (make-local-variable 'completion-styles)
          (setq completion-styles '(orderless basic))
          ;; Ensure Corfu popup works with eglot's completion-at-point
          (setq-local corfu-auto t)))
  
  ;; Keybindings
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c h") #'eglot-signature-help)
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") #'xref-pop-marker-stack)
  (define-key eglot-mode-map (kbd "C-c d") #'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c t") #'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c i") #'eglot-find-implementation))

;; ===================================================================
;; 4. Flymake (built-in) for error display
;; ===================================================================
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! b" . flymake-show-buffer-diagnostics)
         ("C-c ! l" . flymake-show-project-diagnostics)))

;; ===================================================================
;; 5. Optional: better xref integration with Vertico
;; ===================================================================
(use-package xref
  :ensure nil
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; ===================================================================
;; 6. One-time setup: install tree-sitter grammars for your languages
;; ===================================================================
;; Run M-x my/install-all-treesit-grammars if you want to install all at once.
(defun my/install-all-treesit-grammars ()
  "Install tree-sitter grammars for all languages I use."
  (interactive)
  (dolist (lang '(python javascript typescript go rust c cpp css html json bash ruby))
    (my/ensure-treesit-grammar lang)))

(provide 'tool-ide)
;;; tool-ide.el ends here
