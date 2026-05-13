;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; High-performance GC tuning (16GB RAM machine)
;; Very high during startup
(setq gc-cons-threshold (* 300 1024 1024))
(setq gc-cons-percentage 0.6)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-package)
(require 'core-config)
(require 'core-ui)

;; tools
(require 'tool-treemacs)


(message "init.el loaded all modules successfully.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     default))
 '(package-selected-packages
   '(all-the-icons-nerd-fonts doom-modeline doom-themes fira-code-mode
                              nerd-icons-dired treemacs-all-the-icons
                              treemacs-nerd-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
