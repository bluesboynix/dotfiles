;;; lang-scheme.el --- Minimal SLIME-like Scheme REPL -*- lexical-binding: t; -*-

(require 'comint)

(defgroup lang-scheme nil
  "Minimal Scheme REPL integration."
  :group 'languages)

(defcustom lang-scheme-program "guile"
  "Scheme implementation to use."
  :type 'string
  :group 'lang-scheme)

(defcustom lang-scheme-buffer-name "*scheme*"
  "Scheme REPL buffer name."
  :type 'string
  :group 'lang-scheme)

;; ------------------------------------------------------------
;; Minor Mode
;; ------------------------------------------------------------

(defvar lang-scheme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'lang-scheme-run-repl)
    (define-key map (kbd "C-c C-c") #'lang-scheme-send-definition)
    (define-key map (kbd "C-c C-r") #'lang-scheme-send-region)
    (define-key map (kbd "C-c C-b") #'lang-scheme-send-buffer)
    (define-key map (kbd "C-x C-e") #'lang-scheme-eval-last-sexp)
    map))

;;;###autoload
(define-minor-mode lang-scheme-mode
  "Minimal SLIME-like Scheme interaction."
  :lighter " λ"
  :keymap lang-scheme-mode-map)

;; ------------------------------------------------------------
;; Process Management
;; ------------------------------------------------------------

(defun lang-scheme--get-process ()
  (get-buffer-process lang-scheme-buffer-name))

(defun lang-scheme--start-repl ()
  (unless (comint-check-proc lang-scheme-buffer-name)
    (make-comint-in-buffer
     "scheme"
     lang-scheme-buffer-name
     lang-scheme-program)
    (with-current-buffer lang-scheme-buffer-name
      (lang-scheme-repl-mode)))
  (lang-scheme--get-process))

(defun lang-scheme--ensure-visible ()
  (let ((repl (get-buffer lang-scheme-buffer-name))
        (cur (selected-window)))
    (unless (get-buffer-window repl)
      (let ((win (split-window-right)))
        (set-window-buffer win repl)))
    (select-window cur)))

(defun lang-scheme-run-repl ()
  (interactive)
  (lang-scheme--start-repl)
  (lang-scheme--ensure-visible))

;; ------------------------------------------------------------
;; Evaluation (Process-level, stable)
;; ------------------------------------------------------------

(defun lang-scheme--send (string)
  (let ((proc (or (lang-scheme--get-process)
                  (lang-scheme--start-repl))))
    ;; Send directly to process
    (process-send-string proc string)
    (process-send-string proc "\n")))

(defun lang-scheme-send-region (start end)
  (interactive "r")
  (lang-scheme-run-repl)
  (lang-scheme--send
   (buffer-substring-no-properties start end)))

(defun lang-scheme-send-buffer ()
  (interactive)
  (lang-scheme-send-region (point-min) (point-max)))

(defun lang-scheme-send-definition ()
  (interactive)
  (save-excursion
    (mark-defun)
    (lang-scheme-send-region
     (region-beginning)
     (region-end)))
  (deactivate-mark))

(defun lang-scheme-eval-last-sexp ()
  (interactive)
  (lang-scheme-run-repl)
  (lang-scheme--send
   (buffer-substring-no-properties
    (save-excursion (backward-sexp) (point))
    (point))))

;; ------------------------------------------------------------
;; Prompt Fix (Safe Preoutput Filter)
;; ------------------------------------------------------------

(defun lang-scheme--fix-prompts (output)
  "Ensure each Guile prompt starts on a new line."
  (replace-regexp-in-string
   "\\(scheme@[^>]*> \\)"
   "\n\\1"
   output))

;; ------------------------------------------------------------
;; REPL Mode
;; ------------------------------------------------------------

(define-derived-mode lang-scheme-repl-mode comint-mode "Scheme-REPL"
  "Minimal Scheme REPL mode."

  ;; Smooth scrolling
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)

  ;; Prompt detection (no rewriting)
  (setq-local comint-prompt-regexp "^[^>\n]*> ")
  (setq-local comint-use-prompt-regexp t)

  ;; Fix stacked prompts safely
  (add-hook 'comint-preoutput-filter-functions
            #'lang-scheme--fix-prompts
            nil t))

;; ------------------------------------------------------------

(add-hook 'scheme-mode-hook #'lang-scheme-mode)

(provide 'lang-scheme)
;;; lang-scheme.el ends here
