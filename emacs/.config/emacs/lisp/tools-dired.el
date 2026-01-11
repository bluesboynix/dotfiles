;;; tools-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; File management configuration for Dired

;;; Code:

(eval-when-compile
  (require 'dired)
  (require 'dired-x)
  (require 'wdired))

;; -----------------------------
;; Dired
;; -----------------------------
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ;; Navigation
              ("h" . dired-up-directory)
              ("l" . dired-find-file)

              ;; Marking / Selection
              ("m" . dired-mark)
              ("u" . dired-unmark)
              ("U" . dired-unmark-all-marks)
              ("t" . dired-toggle-marks)
              ("*" . dired-mark-files-regexp)

              ;; File operations
              ("C-c c" . dired-do-copy)
              ("C-c r" . dired-do-rename)   ;; rename / move
              ("C-c d" . dired-do-delete)
              ("C-c D" . dired-do-flagged-delete)
              ("C-c n" . dired-create-empty-file)

              ;; View
              ("C-c h" . dired-hide-details-mode))
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; -----------------------------
;; Dired-x (built-in)
;; -----------------------------
(use-package dired-x
  :ensure nil
  :after dired
  :custom
  (dired-omit-files "^\\.\\|^#"))

;; -----------------------------
;; wdired (inline rename)
;; -----------------------------
(use-package wdired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t))

;; -----------------------------
;; dired-subtree (optional, safe)
;; -----------------------------
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode)
  :custom
  (diredfl-global-mode nil))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)


(provide 'tools-dired)
;;; core-dired.el ends here
