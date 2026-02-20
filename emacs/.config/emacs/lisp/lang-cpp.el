;;; lang-cpp.el --- C/C++ configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; C and C++ language configuration.
;; No LSP, no diagnostics, no project logic here.
;; Only language-specific behavior.

;;; Code:

;; ============================================================
;; Tree-sitter Mode (Preferred for Emacs 29+)
;; ============================================================

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c++-mode . c++-ts-mode)))

;; ============================================================
;; C/C++ Style Configuration
;; ============================================================

(defun lang-cpp-setup ()
  "C/C++ local configuration."

  ;; 2-space indentation
  (setq-local c-basic-offset 2)

  ;; Use Linux style as base
  (c-set-style "linux")

  ;; No tabs
  (setq-local indent-tabs-mode nil)

  ;; Better compilation error parsing
  (setq-local compilation-read-command nil)

  ;; Auto newline after braces
  (c-toggle-auto-newline 1)

  ;; Clean trailing whitespace only on save
  (add-hook 'before-save-hook
            (lambda ()
              (delete-trailing-whitespace))
            nil t))

(add-hook 'c-mode-hook #'lang-cpp-setup)
(add-hook 'c++-mode-hook #'lang-cpp-setup)

;; ============================================================
;; File Associations
;; ============================================================

(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'"  . c++-mode))

;; ============================================================
;; Provide
;; ============================================================

(provide 'lang-cpp)
;;; lang-cpp.el ends here
