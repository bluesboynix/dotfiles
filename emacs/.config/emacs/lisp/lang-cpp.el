;;; lang-cpp.el --- Minimal C/C++ setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern C/C++ configuration using tree-sitter + eglot (Emacs 29+)
;; Requires: clangd installed on system.

;;; Code:

;; --------------------------------------------------
;; Tree-sitter C / C++
;; --------------------------------------------------
(when (treesit-available-p)

  ;; Associate file extensions
  (add-to-list 'major-mode-remap-alist '(c-mode  . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

  (use-package c-ts-mode
    :ensure nil
    :mode "\\.c\\'"
    :hook (c-ts-mode . my/cpp-mode-setup))

  (use-package c++-ts-mode
    :ensure nil
    :mode "\\.cpp\\'"
    :hook (c++-ts-mode . my/cpp-mode-setup)))

;; --------------------------------------------------
;; Common C/C++ Setup
;; --------------------------------------------------
(defun my/cpp-mode-setup ()
  "Minimal C/C++ defaults."
  (setq-local c-basic-offset 4)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (eglot-ensure))

;; --------------------------------------------------
;; Eglot (LSP)
;; --------------------------------------------------
(use-package eglot
  :ensure nil
  :commands eglot eglot-ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t))

;; --------------------------------------------------
;; Optional: Clang-format on save
;; --------------------------------------------------
(use-package clang-format
  :ensure t
  :hook ((c-ts-mode c++-ts-mode) . clang-format-on-save-mode))

(provide 'lang-cpp)
;;; lang-cpp.el ends here
