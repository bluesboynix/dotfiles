;;; ui-dashboard.el --- Minimal Emacs Startup Dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Clean, minimal dashboard with recent files, projects, and bookmarks.

;;; Code:

(use-package dashboard
  :ensure t
  :demand t
  :custom
  ;; Use a valid banner type
  (dashboard-startup-banner 'official)
  
  ;; Hide banner by setting logo title to nil and overriding the insert
  (dashboard-banner-logo-title nil)
  
  ;; Layout
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  
  ;; Sections
  (dashboard-items '((recents   . 12)
                     (projects  . 8)
                     (bookmarks . 8)))
  
  ;; Section titles
  (dashboard-item-names '((recents   . "Recent Files:")
                          (projects  . "Projects:")
                          (bookmarks . "Bookmarks:")))
  
  ;; No icons
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-set-navigator nil)
  
  ;; Footer
  (dashboard-footer t)
  (dashboard-footer-messages
   '("Open file: C-x C-f    "
     "Magit: C-x g    "
     "Treemacs: F8    "
     "Terminal: F9"))
  
  ;; Formatting
  (dashboard-show-filenames t)
  (dashboard-file-length 40)
  (dashboard-projects-backend 'projectile)
  
  :config
  ;; Simple banner override
  (advice-add 'dashboard-insert-banner :override #'ignore)
  
  ;; Initialize
  (dashboard-setup-startup-hook)
  
  ;; Open dashboard manually
  (defun my/dashboard ()
    (interactive)
    (dashboard-open))
  
  (global-set-key (kbd "C-c d") #'my/dashboard))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
