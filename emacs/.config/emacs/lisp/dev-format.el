;;; dev-format.el --- Formatting layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Formatting abstraction layer.
;; Uses Eglot if available.
;; Safe, optional, and extensible.

;;; Code:

;; ============================================================
;; Core Formatter
;; ============================================================

(defun dev-format-buffer ()
  "Format current buffer using the best available formatter."
  (interactive)
  (cond
   ;; Use Eglot if managing this buffer
   ((and (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    (eglot-format-buffer))

   ;; Fallback: indent whole buffer
   (t
    (indent-region (point-min) (point-max)))))

;; ============================================================
;; Optional: Format on Save (Disabled by Default)
;; ============================================================

(defvar dev-format-on-save nil
  "If non-nil, format buffer automatically before saving.")

(defun dev-format-maybe ()
  "Format buffer if `dev-format-on-save` is enabled."
  (when dev-format-on-save
    (dev-format-buffer)))

(add-hook 'before-save-hook #'dev-format-maybe)

;; ============================================================
;; Manual Keybinding (Global, Safe)
;; ============================================================

(global-set-key (kbd "C-c f") #'dev-format-buffer)

(provide 'dev-format)
;;; dev-format.el ends here
