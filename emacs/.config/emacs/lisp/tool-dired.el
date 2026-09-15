;;; tool-dired.el --- Dired as a general file manager -*- lexical-binding: t; -*-

;;; Commentary:
;; Make Dired behave more like a traditional file manager while
;; keeping everything built-in to Emacs.

;;; Code:

(require 'dired)
(require 'dired-x)
(require 'wdired)

;; Appearance
;; Show files/directories without the noisy permission/owner information.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Human-readable sizes, directories first, case-insensitive sorting.
(setq dired-listing-switches "-alh --group-directories-first")

;; Keep one Dired buffer per directory where possible.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Reuse the existing Dired buffer when entering subdirectories.
(setq dired-auto-revert-buffer t)

;; Automatically refresh Dired when files change on disk.
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; File operations
;; When copying/moving, use the other visible Dired window as the
;; destination when possible.
(setq dired-dwim-target t)

;; Ask before recursively copying directories.
(setq dired-recursive-copies 'always)

;; Ask before recursively deleting directories.
(setq dired-recursive-deletes 'always)

;; Preserve timestamps when copying.
(setq dired-copy-preserve-time t)

;; Create missing destination directories when necessary.
(setq dired-create-destination-dirs 'ask)

(defun my/dired-sort-directories-files-hidden ()
  "Sort Dired with directories first, normal files second, hidden files last."
  (interactive)
  (setq-local dired-listing-switches
              "-alh --group-directories-first")
  (revert-buffer))

;; Navigation
(defun my/dired-up-directory ()
  "Go to the parent directory."
  (interactive)
  (find-alternate-file ".."))

(defun my/dired-find-file ()
  "Open the file or enter the directory at point."
  (interactive)
  (dired-find-file))

;; External file opening
(defun my/dired-open-external ()
  "Open the file at point using the system's default application."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (start-process "dired-open-external"
                   nil
                   "xdg-open"
                   file)))

;; File manager keybindings
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") #'dired-find-file)
  (define-key dired-mode-map (kbd "SPC") #'dired-mark)
  (define-key dired-mode-map (kbd "DEL") #'dired-unmark-backward)
  (define-key dired-mode-map (kbd "u") #'dired-unmark)

  ;; Navigation
  (define-key dired-mode-map (kbd "h") #'my/dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file)
  (define-key dired-mode-map (kbd "n") #'dired-next-line)
  (define-key dired-mode-map (kbd "p") #'dired-previous-line)

  ;; File operations
  (define-key dired-mode-map (kbd "c") #'dired-do-copy)
  (define-key dired-mode-map (kbd "m") #'dired-do-rename)
  (define-key dired-mode-map (kbd "d") #'dired-flag-file-deletion)
  (define-key dired-mode-map (kbd "x") #'dired-do-flagged-delete)

  ;; Marking
  (define-key dired-mode-map (kbd "a") #'dired-mark)
  (define-key dired-mode-map (kbd "A") #'dired-mark-files-regexp)
  (define-key dired-mode-map (kbd "U") #'dired-unmark-all-marks)

  ;; Refresh
  (define-key dired-mode-map (kbd "g") #'revert-buffer)

  ;; Toggle details
  (define-key dired-mode-map (kbd "(") #'dired-hide-details-mode)

  ;; Edit filenames directly
  (define-key dired-mode-map (kbd "C-c C-r") #'wdired-change-to-wdired-mode)

  ;; Open using the desktop environment
  (define-key dired-mode-map (kbd "o") #'my/dired-open-external)

  ;; Go to parent directory
  (define-key dired-mode-map (kbd "^") #'my/dired-up-directory))

;; Dired-X
;; Show omitted files with C-x M-o.
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\.git$"
              "\\|^\\.gitignore$"))

;;;###autoload
(defun my/dired-setup ()
  "Apply personal Dired settings."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook #'my/dired-setup)

(provide 'tool-dired)

;;; tool-dired.el ends here
