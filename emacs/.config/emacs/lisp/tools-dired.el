;;; tools-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; File management configuration for Dired with enhanced features.

;;; Code:

(eval-when-compile
  (require 'dired)
  (require 'dired-x)
  (require 'wdired))

;; ----------------------------------------------------------------------
;; Core Dired Configuration
;; ----------------------------------------------------------------------
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ;; Navigation (vi-style)
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ("J" . dired-goto-file)  ; Jump to file

              ;; Marking / Selection
              ("m" . dired-mark)
              ("u" . dired-unmark)
              ("U" . dired-unmark-all-marks)
              ("t" . dired-toggle-marks)
              ("*" . dired-mark-files-regexp)
              ("%" . dired-mark-files-regexp)   ; Alternative for regex mark

              ;; File operations
              ("C-c c" . dired-do-copy)
              ("C-c r" . dired-do-rename)
              ("C-c d" . dired-do-delete)
              ("C-c D" . dired-do-flagged-delete)
              ("C-c n" . dired-create-empty-file)
              ("C-c R" . dired-do-rename-regexp)  ; Regex rename

              ;; Permissions
              ("C-c M" . dired-do-chmod)         ; Change mode

              ;; View
              ("C-c h" . dired-hide-details-mode)

              ;; Search
              ("C-c s" . dired-do-search)        ; Search files
              ("C-c Q" . dired-do-query-replace) ; Query replace in files
              )

  :custom
  ;; Listing options
  (dired-listing-switches "-alh --group-directories-first -v")  ; Version sort
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)  ; Auto-revert
  (dired-dwim-target t)  ; Suggest target directory for copy/rename

  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; ----------------------------------------------------------------------
;; Dired-x (built-in extras)
;; ----------------------------------------------------------------------
(use-package dired-x
  :after dired
  :ensure nil
  :custom
  (dired-omit-files "^\\.\\|^#\\|~$")  ; Omit dotfiles, backups, temp
  (dired-omit-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers t)

  :config
  ;; Enable omitting by default
  (setq-default dired-omit-mode t)

  ;; Add X features
  (define-key dired-mode-map (kbd "C-c o") #'dired-omit-mode))

;; ----------------------------------------------------------------------
;; wdired (inline rename)
;; ----------------------------------------------------------------------
(use-package wdired
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode)
              ("C-x C-q" . wdired-change-to-wdired-mode))  ; Traditional binding
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-allow-to-redirect-links t))

;; ----------------------------------------------------------------------
;; Async operations (dired-async is part of async package)
;; ----------------------------------------------------------------------
(use-package async
  :config
  ;; Enable async compilation of packages
  (async-bytecomp-package-mode 1)

  ;; dired-async is part of async - require it and enable
  (require 'dired-async nil t)  ; nil = no error if missing, t = noecho
  (when (fboundp 'dired-async-mode)
    (dired-async-mode 1))

  ;; Message about async status
  (message "Async mode enabled for dired operations"))

;; ----------------------------------------------------------------------
;; dired-subtree (tree-style expansion)
;; ----------------------------------------------------------------------
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)  ; Cycle through subdirs
              ("C-<tab>" . dired-subtree-remove))  ; Remove all subdirs
  :config
  ;; Don't insert subtree when entering directory
  (setq dired-subtree-use-backgrounds t))

;; ----------------------------------------------------------------------
;; Icons
;; ----------------------------------------------------------------------
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

;; ----------------------------------------------------------------------
;; Color highlighting
;; ----------------------------------------------------------------------
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode)
  :custom
  (diredfl-global-mode nil))

;; ----------------------------------------------------------------------
;; Additional dired enhancements (optional)
;; ----------------------------------------------------------------------
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("C-c /" . dired-narrow)
              ("C-c f" . dired-narrow-fuzzy)))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions
        '(("png" . "feh")
          ("jpg" . "feh")
          ("jpeg" . "feh")
          ("gif" . "feh")
          ("mp4" . "mpv")
          ("mkv" . "mpv")
          ("pdf" . "evince")
          ("docx" . "libreoffice")
          ("xlsx" . "libreoffice"))))

(use-package dired-quick-sort
  :after dired
  :bind (:map dired-mode-map
              ("C-c s" . dired-quick-sort)))

;; ----------------------------------------------------------------------
;; Hide details by default
;; ----------------------------------------------------------------------
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; ----------------------------------------------------------------------
;; Custom functions
;; ----------------------------------------------------------------------
(defun my/dired-copy-filename ()
  "Copy filename of current file to kill ring."
  (interactive)
  (let ((filename (dired-get-filename)))
    (kill-new (file-name-nondirectory filename))
    (message "Copied: %s" (file-name-nondirectory filename))))

(defun my/dired-copy-file-path ()
  "Copy full file path to kill ring."
  (interactive)
  (let ((filename (dired-get-filename)))
    (kill-new (expand-file-name filename))
    (message "Copied path: %s" (expand-file-name filename))))

;; Add these to dired mode map
(define-key dired-mode-map (kbd "C-c y") #'my/dired-copy-filename)
(define-key dired-mode-map (kbd "C-c Y") #'my/dired-copy-file-path)

;; ----------------------------------------------------------------------
;; File renaming with regex
;; ----------------------------------------------------------------------
(defun my/dired-rename-regexp (from to)
  "Rename files matching regex FROM to pattern TO."
  (interactive
   (let* ((from (read-regexp "Rename files matching: "))
          (to (read-string (format "Rename %s to pattern: " from))))
     (list from to)))
  (dired-rename-regexp from to))

(define-key dired-mode-map (kbd "C-c R") #'my/dired-rename-regexp)

(message "Dired configuration loaded successfully.")
(provide 'tools-dired)
;;; tools-dired.el ends here
