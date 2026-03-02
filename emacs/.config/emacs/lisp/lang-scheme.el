;;; lang-scheme.el --- Minimal Scheme REPL integration -*- lexical-binding: t; -*-

(require 'comint)

(defgroup lang-scheme nil
  "Minimal Scheme REPL integration."
  :group 'languages)

(defcustom lang-scheme-program "guile"
  "Program used to run Scheme REPL."
  :type 'string
  :group 'lang-scheme)

(defcustom lang-scheme-buffer-name "*scheme*"
  "Name of Scheme REPL buffer."
  :type 'string
  :group 'lang-scheme)

(defvar lang-scheme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'lang-scheme-switch-to-repl)
    (define-key map (kbd "C-c C-c") #'lang-scheme-send-definition)
    (define-key map (kbd "C-c C-r") #'lang-scheme-send-region)
    (define-key map (kbd "C-c C-b") #'lang-scheme-send-buffer)
    map)
  "Keymap for lang-scheme minor mode.")

;;;###autoload
(define-minor-mode lang-scheme-mode
  "Minor mode for interacting with a Scheme REPL."
  :lighter " Scheme"
  :keymap lang-scheme-mode-map)

(defun lang-scheme--get-process ()
  "Return active Scheme process or nil."
  (get-buffer-process lang-scheme-buffer-name))

(defun lang-scheme--start-repl ()
  "Start Scheme REPL if not running."
  (unless (comint-check-proc lang-scheme-buffer-name)
    (let ((buffer (apply #'make-comint
                         "scheme"
                         lang-scheme-program
                         nil
                         nil)))
      (with-current-buffer buffer
        (lang-scheme-repl-mode))))
  (get-buffer-process lang-scheme-buffer-name))

(defun lang-scheme-run-repl ()
  "Start or switch to Scheme REPL."
  (interactive)
  (lang-scheme--start-repl)
  (pop-to-buffer lang-scheme-buffer-name))

(defun lang-scheme-switch-to-repl ()
  "Switch to Scheme REPL buffer."
  (interactive)
  (if (lang-scheme--get-process)
      (pop-to-buffer lang-scheme-buffer-name)
    (lang-scheme-run-repl)))

(defun lang-scheme--send-string (string)
  "Send STRING to Scheme REPL and evaluate it cleanly."
  (let ((proc (or (lang-scheme--get-process)
                  (lang-scheme--start-repl))))
    (unless (string-suffix-p "\n" string)
      (setq string (concat string "\n")))
    (comint-send-string proc string)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (recenter -1))))

(defun lang-scheme-send-region (start end)
  "Send region between START and END to Scheme REPL."
  (interactive "r")
  (lang-scheme--send-string
   (buffer-substring-no-properties start end)))

(defun lang-scheme-send-buffer ()
  "Send entire buffer to Scheme REPL."
  (interactive)
  (lang-scheme-send-region (point-min) (point-max)))

(defun lang-scheme-send-definition ()
  "Send current top-level definition to Scheme REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (lang-scheme-send-region
     (region-beginning)
     (region-end))))

;;; REPL Mode

(define-derived-mode lang-scheme-repl-mode comint-mode "Scheme-REPL"
  "Major mode for Scheme REPL."
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))


(add-hook 'scheme-mode-hook #'lang-scheme-mode)


(provide 'lang-scheme)
;;; lang-scheme.el ends here
