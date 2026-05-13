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
(require 'tool-completions)
(require 'tool-treemacs)

(message "init.el loaded all modules successfully.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline doom-themes fira-code-mode nerd-icons-dired
                   treemacs-icons-dired treemacs-nerd-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
