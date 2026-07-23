;;; lang-scheme.el --- Guile REPL -*- lexical-binding: t; -*-

(require 'comint)

(defvar guile-repl-buffer "*guile*")

(defun run-guile ()
  "Start Guile REPL."
  (interactive)
  (let ((buffer (get-buffer guile-repl-buffer)))
    (if (and buffer (comint-check-proc buffer))
        (pop-to-buffer buffer)
      (pop-to-buffer
       (make-comint "guile" "guile")))))

(defun guile-send-region (start end)
  "Send region to Guile."
  (interactive "r")
  (let ((proc (get-buffer-process "*guile*")))
    (unless proc
      (run-guile)
      (setq proc (get-buffer-process "*guile*")))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")))

(defun guile-send-buffer ()
  "Send buffer to Guile."
  (interactive)
  (guile-send-region (point-min) (point-max)))

(add-to-list 'auto-mode-alist
             '("\\.scm\\'" . scheme-mode))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-r")
              #'guile-send-region)
  (define-key scheme-mode-map (kbd "C-c C-b")
              #'guile-send-buffer))

(provide 'lang-scheme)
