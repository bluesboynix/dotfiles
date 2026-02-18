;;; tools-git.el --- Git integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git tools including Magit, diff-hl, and related utilities.

;;; Code:

;; ----------------------------------------------------------------------
;; Magit: The Git Porcelain
;; ----------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-blame magit-log-buffer-file)
  :bind (("C-x g" . magit-status)           ; Quick status
         ("C-x M-g" . magit-dispatch)        ; Magit command menu
         ("C-c g s" . magit-status)          ; Alternative
         ("C-c g b" . magit-blame)           ; Blame current file
         ("C-c g l" . magit-log-buffer-file) ; Log current file
         ("C-c g f" . magit-file-dispatch))  ; File-specific commands
  :custom
  ;; Display behavior
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'magit-mode-quit-window)
  
  ;; Status buffer headers
  (magit-status-headers-hook '(magit-insert-error-header
                               magit-insert-diff-filter-header
                               magit-insert-head-branch-header
                               magit-insert-upstream-branch-header
                               magit-insert-tags-header))
  
  ;; Log and diff settings
  (magit-refs-show-commit-count 'all)
  (magit-log-arguments '("--graph" "--decorate" "-n256"))
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace nil)  ; Don't highlight whitespace (optional)
  
  ;; Section visibility
  (magit-section-initial-visibility-alist
   '((stashes . hide)
     (unpushed . show)
     (unpulled . show)
     (untracked . show)
     (unstaged . show)
     (staged . show)))
  
  :config
  ;; Truncate long lines in diff buffers
  (add-hook 'magit-diff-mode-hook #'visual-line-mode)
  
  ;; Use better completion
  (setq magit-completing-read-function 'magit-builtin-completing-read)
  
  ;; Save repository state occasionally
  (setq magit-save-repository-buffers 'dontask)
  
  ;; Don't auto-revert repositories
  (setq magit-revert-buffers nil))

;; ----------------------------------------------------------------------
;; Inline Git changes in fringe
;; ----------------------------------------------------------------------
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Show changes as you edit
  (diff-hl-flydiff-mode 1)
  
  ;; Terminal support (no fringe)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  
  ;; Navigation keybindings
  (global-set-key (kbd "C-x v n") 'diff-hl-next-hunk)
  (global-set-key (kbd "C-x v p") 'diff-hl-previous-hunk)
  (global-set-key (kbd "C-x v r") 'diff-hl-revert-hunk)
  
  ;; FIXED: Use the correct hook name or condition
  (with-eval-after-load 'vc
    (when (boundp 'vc-find-file-hook)
      (add-to-list 'vc-find-file-hook 'diff-hl-mode))
    ;; Alternative for older Emacs versions
    (add-hook 'find-file-hook 'diff-hl-mode t))
  
  ;; Optional: Show in dired
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  )

;; ----------------------------------------------------------------------
;; Git Link: Get GitHub/GitLab links to current line
;; ----------------------------------------------------------------------
(use-package git-link
  :ensure t
  :bind ("C-c g l" . git-link)
  :custom
  (git-link-open-in-browser t))

;; ----------------------------------------------------------------------
;; Browse at remote
;; ----------------------------------------------------------------------
(use-package browse-at-remote
  :ensure t
  :defer t
  :bind ("C-c g o" . browse-at-remote))

;; ----------------------------------------------------------------------
;; Git Timemachine: View file history
;; ----------------------------------------------------------------------
(use-package git-timemachine
  :ensure t
  :defer t
  :bind ("C-c g t" . git-timemachine)
  :custom
  (git-timemachine-show-minibuffer-details t))

;; ----------------------------------------------------------------------
;; FIXED: Git Commit message improvements - git-commit is part of magit!
;; ----------------------------------------------------------------------
(use-package git-commit
  :ensure nil  ; Don't try to install - it's part of magit
  :after magit
  :hook (git-commit-mode . (lambda ()
                            (setq fill-column 72)
                            (flyspell-mode 1))))

;; ----------------------------------------------------------------------
;; Better merge conflict resolution
;; ----------------------------------------------------------------------
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; ----------------------------------------------------------------------
;; Git modes for various Git files
;; ----------------------------------------------------------------------
(use-package git-modes
  :ensure t
  :defer t)

(message "Git integration loaded successfully.")
(provide 'tools-git)
;;; tools-git.el ends here
