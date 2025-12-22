;;; ui-dashboard.el --- Polished Emacs Startup Dashboard -*- lexical-binding: t; -*-

(use-package dashboard
  :ensure t
  :config
  ;; Enable dashboard at startup
  (dashboard-setup-startup-hook)

  ;; Banner / welcome message
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'official) ;; or a path to ASCII/banner file
  (setq dashboard-center-content t)

  ;; Sections: recent files, projects, bookmarks
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5)))
  (setq dashboard-item-names '((recents  . "Recent Files:")
                               (projects . "Projects:")
                               (bookmarks . "Bookmarks:")))

  ;; Footer / shortcut hints
  (setq dashboard-footer-messages
        '("Shortcuts: C-x C-f Open file | C-x g Magit | F8 treemacs | F9 Terminal"))

  ;; Enable icons (requires all-the-icons package)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; Custom widget: loaded language modules
  (defun dashboard-insert-languages-widget (list-size)
    "Insert a nice list of loaded language modules."
    (insert "\n;; Loaded Languages:\n")
    (insert ";;  • C/C++\n")
    (insert ";;  • Python\n")
    (insert ";;  • Rust\n")
    (insert ";;  • Scheme")
    (insert ";;  • Racket")
    (insert ";;  • Common Lisp\n\n"))

  ;; Add custom languages widget before recents
  (add-to-list 'dashboard-item-generators
               '(languages . dashboard-insert-languages-widget))
  (add-to-list 'dashboard-items '(languages) t))

;; Optional: ensure all-the-icons is available for icons
(use-package all-the-icons
  :ensure t)

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
