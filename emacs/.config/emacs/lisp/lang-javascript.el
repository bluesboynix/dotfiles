;;; lang-javascript.el --- JavaScript / Node.js setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-focused JavaScript setup
;; - js-ts-mode (Tree-sitter)
;; - typescript-ts-mode
;; - json-ts-mode
;; - Eglot (LSP)
;; - Prettier formatting
;; - npm integration

;;; Code:

;; =========================
;; JavaScript (Tree-sitter)
;; =========================

(when (treesit-available-p)

  (use-package js-ts-mode
    :ensure nil
    :mode ("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'")
    :hook ((js-ts-mode . eglot-ensure)
           (js-ts-mode . prettify-symbols-mode))
    :config
    (setq js-indent-level 2))

  ;; =========================
  ;; TypeScript (Optional but recommended)
  ;; =========================

  (use-package typescript-ts-mode
    :ensure nil
    :mode ("\\.ts\\'" "\\.tsx\\'")
    :hook (typescript-ts-mode . eglot-ensure)
    :config
    (setq typescript-indent-level 2))

  ;; =========================
  ;; JSON
  ;; =========================

  (use-package json-ts-mode
    :ensure nil
    :mode "\\.json\\'"
    :hook (json-ts-mode . eglot-ensure)))

;; =========================
;; Eglot Configuration
;; =========================

(use-package eglot
  :ensure nil
  :config
  ;; Use vscode-langservers-extracted or typescript-language-server
  (add-to-list 'eglot-server-programs
               '((js-ts-mode
                  typescript-ts-mode
                  json-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

;; =========================
;; Prettier Formatting
;; =========================

(use-package prettier-js
  :ensure t
  :hook ((js-ts-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (json-ts-mode . prettier-js-mode)))

;; =========================
;; npm / Node Helpers
;; =========================

(use-package npm-mode
  :ensure t
  :hook ((js-ts-mode . npm-mode)
         (typescript-ts-mode . npm-mode)))

;; =========================
;; Useful Keybindings
;; =========================

(with-eval-after-load 'js-ts-mode
  (define-key js-ts-mode-map (kbd "C-c C-r") #'nodejs-repl-send-region)
  (define-key js-ts-mode-map (kbd "C-c C-b") #'nodejs-repl-send-buffer))

;; =========================
;; Optional: Node REPL
;; =========================

(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl nodejs-repl-send-region nodejs-repl-send-buffer))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
