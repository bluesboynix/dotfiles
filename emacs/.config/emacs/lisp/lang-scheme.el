;;; lang-scheme.el --- Generic Pretty Scheme REPL -*- lexical-binding: t; -*-

(require 'comint)
(require 'scheme)

(defgroup lang-scheme nil "" :group 'languages)

(defcustom lang-scheme-backend 'guile
  ""
  :type '(choice (const guile) (const gambit) (const chicken))
  :group 'lang-scheme)

(defcustom lang-scheme-window-width 0.38
  ""
  :type 'float
  :group 'lang-scheme)

(defcustom lang-scheme-backends
  '((guile   :program "guile" :buffer "*scheme-guile*")
    (gambit  :program "gsi"   :buffer "*scheme-gambit*")
    (chicken :program "csi"   :buffer "*scheme-chicken*"))
  ""
  :type 'alist
  :group 'lang-scheme)

(defun lang-scheme--cfg ()
  (alist-get lang-scheme-backend lang-scheme-backends))

(defun lang-scheme--program ()
  (plist-get (lang-scheme--cfg) :program))

(defun lang-scheme--buffer ()
  (plist-get (lang-scheme--cfg) :buffer))

(defun lang-scheme--ensure-process ()
  (let* ((buf-name (lang-scheme--buffer))
         (buf (or (get-buffer buf-name)
                  (make-comint-in-buffer
                   "scheme" buf-name (lang-scheme--program)))))
    (or (get-buffer-process buf)
        (with-current-buffer buf
          (make-comint-in-buffer
           "scheme" buf-name (lang-scheme--program))
          (get-buffer-process buf)))))

(defun lang-scheme--display ()
  (display-buffer-in-side-window
   (get-buffer-create (lang-scheme--buffer))
   `((side . right)
     (slot . 0)
     (window-width . ,lang-scheme-window-width))))

(defun lang-scheme--send (string)
  (let ((proc (lang-scheme--ensure-process)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n")))
    (comint-send-string proc string)
    (comint-send-string proc "\n")
    (when-let ((win (get-buffer-window (lang-scheme--buffer))))
      (with-selected-window win
        (goto-char (point-max))))))

(defun lang-scheme-run ()
  (interactive)
  (lang-scheme--ensure-process)
  (lang-scheme--display))

(defun lang-scheme-send-region (beg end)
  (interactive "r")
  (lang-scheme--send
   (buffer-substring-no-properties beg end)))

(defun lang-scheme-send-buffer ()
  (interactive)
  (lang-scheme-send-region (point-min) (point-max)))

(defun lang-scheme-send-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (lang-scheme-send-region
     (region-beginning)
     (region-end)))
  (deactivate-mark))

(defun lang-scheme-clear ()
  (interactive)
  (when-let ((buf (get-buffer (lang-scheme--buffer))))
    (with-current-buffer buf
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))))

(defun lang-scheme-restart ()
  (interactive)
  (when-let ((proc (get-buffer-process (lang-scheme--buffer))))
    (delete-process proc))
  (kill-buffer (lang-scheme--buffer))
  (lang-scheme-run))

(defun lang-scheme-switch-backend (backend)
  (interactive
   (list (intern
          (completing-read
           "Backend: "
           (mapcar #'car lang-scheme-backends)))))
  (setq lang-scheme-backend backend)
  (message "Scheme backend → %s" backend))

(defun lang-scheme--style ()
  (setq-local comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output t
              comint-input-ignoredups t
              comint-prompt-regexp "^> "
              comint-use-prompt-regexp t)

  ;; force clean simple prompt
  (add-hook
   'comint-output-filter-functions
   (lambda (output)
     (when (string-match "^[^>\n]*>" output)
       (replace-match "> " nil nil output)))
   nil t)

  ;; faces
  (set-face-attribute 'comint-highlight-prompt nil
                      :weight 'bold
                      :foreground "#ff79c6")

  (set-face-attribute 'comint-input nil
                      :foreground "#8be9fd"))

(add-hook 'comint-mode-hook #'lang-scheme--style)

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-z") #'lang-scheme-run)
  (define-key scheme-mode-map (kbd "C-c C-c") #'lang-scheme-send-defun)
  (define-key scheme-mode-map (kbd "C-c C-r") #'lang-scheme-send-region)
  (define-key scheme-mode-map (kbd "C-c C-b") #'lang-scheme-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-l") #'lang-scheme-clear)
  (define-key scheme-mode-map (kbd "C-c C-k") #'lang-scheme-restart)
  (define-key scheme-mode-map (kbd "C-c C-s") #'lang-scheme-switch-backend))

(provide 'lang-scheme)
