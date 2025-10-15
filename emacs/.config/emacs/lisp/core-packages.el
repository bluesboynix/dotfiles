;;; core-packages.el --- Package management setup -*- lexical-binding: t; -*-

;; Disable package.el automatic loading at startup
(setq package-enable-at-startup nil)

;; Initialize package.el manually
(require 'package)

;; Add the main ELPA repositories
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Ensure package archives are initialized
(unless package--initialized
  (package-initialize))

;; Refresh archive contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Always ensure packages get installed automatically
(setq use-package-always-ensure t)

(provide 'core-packages)
