;;; lisp-dev.el --- Lisp & Scheme Development Config

;; ======================================================
;; Common Keymap Setup
;; ======================================================

(defvar lisp-dev-repl-map (make-sparse-keymap)
  "Common keymap for Lisp/Scheme REPL interactions.")

(define-key lisp-dev-repl-map (kbd "C-c C-z") #'ignore) ;; repl
(define-key lisp-dev-repl-map (kbd "C-c C-c") #'ignore) ;; send defun
(define-key lisp-dev-repl-map (kbd "C-c C-r") #'ignore) ;; send region
(define-key lisp-dev-repl-map (kbd "C-c C-b") #'ignore) ;; send buffer
(define-key lisp-dev-repl-map (kbd "C-c C-k") #'ignore) ;; load/run file

(defun lisp-dev-apply-keys (map repl defun region buffer file)
  (define-key map (kbd "C-c C-z") repl)
  (define-key map (kbd "C-c C-c") defun)
  (define-key map (kbd "C-c C-r") region)
  (define-key map (kbd "C-c C-b") buffer)
  (define-key map (kbd "C-c C-k") file))

;; ======================================================
;; Common Lisp (SLIME + SBCL)
;; ======================================================
(use-package slime
  :ensure t
  :mode ("\\.lisp\\'" . lisp-mode)
  :commands (slime slime-connect)
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy))
  (with-eval-after-load 'slime
    (lisp-dev-apply-keys slime-mode-map
                         #'slime
                         #'slime-compile-defun
                         #'slime-compile-region
                         #'slime-compile-buffer
                         #'slime-load-file)))

;; ======================================================
;; Gambit Scheme (Comint Mode)
;; ======================================================
(defun run-gambit ()
  "Run Gambit Scheme REPL in a comint buffer."
  (interactive)
  (unless (comint-check-proc "*gambit*")
    (set-buffer (make-comint "gambit" "gsi")))
  (pop-to-buffer-same-window "*gambit*"))

(defun gambit--send-and-return (string)
  (unless (comint-check-proc "*gambit*")
    (set-buffer (make-comint "gambit" "gsi")))
  (with-current-buffer "*gambit*"
    (goto-char (point-max))
    (insert string)
    (comint-send-input)))

(defun gambit-send-region (start end)
  (interactive "r")
  (gambit--send-and-return (buffer-substring-no-properties start end)))

(defun gambit-send-buffer ()
  (interactive)
  (gambit-send-region (point-min) (point-max)))

(defun gambit-send-definition ()
  (interactive)
  (save-excursion
    (mark-defun)
    (gambit-send-region (region-beginning) (region-end)))
  (deactivate-mark))

(defun gambit-load-file ()
  (interactive)
  (when buffer-file-name
    (gambit--send-and-return (format "(load \"%s\")" buffer-file-name))))

(with-eval-after-load 'scheme
  (lisp-dev-apply-keys scheme-mode-map
                       #'run-gambit
                       #'gambit-send-definition
                       #'gambit-send-region
                       #'gambit-send-buffer
                       #'gambit-load-file))

;; ======================================================
;; MIT Scheme (cmuscheme)
;; ======================================================
(use-package cmuscheme
  :ensure nil ;; built-in
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (setq scheme-program-name "mit-scheme")

  (defun run-mit-scheme ()
    (interactive)
    (unless (comint-check-proc "*scheme*")
      (set-buffer (make-comint "scheme" scheme-program-name)))
    (pop-to-buffer-same-window "*scheme*"))

  (with-eval-after-load 'scheme
    (lisp-dev-apply-keys scheme-mode-map
                         #'run-mit-scheme
                         #'scheme-send-definition
                         #'scheme-send-region
                         #'scheme-send-buffer
                         #'scheme-load-file)))

(provide 'lisp-dev)
;;; lisp-dev.el ends here
