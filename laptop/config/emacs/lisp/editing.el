;; ====================
;; Editing Enhancements
;; ====================
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)  ; Highlight matching parentheses
(delete-selection-mode 1)  ; Override selected text on typing

;; Backup Files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(provide 'editing)
