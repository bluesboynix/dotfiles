;; lsp mode
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (c-mode . lsp)
	 (go-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (sh-mode . lsp))
  :config
  (setq lsp-completion-provider :capf))
  ;;(setq lsp-prefer-flymake nil)) ; use flycheck instead

;; install the following in system
;; pyright, clang, yaml-language-server, bash-language-server

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

;; tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt) ;; or 'always
  (global-treesit-auto-mode))

;; Remap scheme-mode to scheme-ts-mode if Tree-sitter is available
(add-to-list 'major-mode-remap-alist
             '(scheme-mode . scheme-ts-mode))

;; Better font lock and optional folding
(setq treesit-font-lock-level 4)
(setq treesit-fold-enable t)

(provide 'lsp-config)
