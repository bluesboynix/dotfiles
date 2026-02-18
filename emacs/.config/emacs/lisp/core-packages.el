;;; core-packages.el --- Package management setup -*- lexical-binding: t; -*-

;; Disable package.el automatic loading at startup
(setq package-enable-at-startup nil)

;; Initialize package.el manually
(require 'package)

(setq package-archives
	  '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
		("MELPA"        . "https://melpa.org/packages/")
		("ORG"          . "https://orgmode.org/elpa/")
		("MELPA Stable" . "https://stable.melpa.org/packages/")
		("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
	  package-archive-priorities
	  '(("GNU ELPA"     . 20)
		("MELPA"        . 15)
		("ORG"          . 10)
		("MELPA Stable" . 5)
		("nongnu"       . 0)))

;; Ensure package archives are initialized
(package-initialize)

;; Refresh archive contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Ensure use-package is available
(require 'use-package)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure packages get installed automatically
(setq use-package-always-ensure t)

(provide 'core-packages)
