;;; core-package.el --- Custom package setup -*- lexical-binding: t; -*-

;; Do NOT (require 'package) here – it's already built-in

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-refresh-contents-if-time-critical t)

(unless (package-installed-p 'use-package)
  (message "use-package not found. Run M-x package-refresh-contents, then M-x package-install use-package"))

;; Helper: install a package if missing
(defun ensure-package (pkg)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Install required packages
(ensure-package 'doom-themes)
(ensure-package 'doom-modeline)
(ensure-package 'fira-code-mode)

;; Optional: all-the-icons for richer modeline icons
(ensure-package 'all-the-icons)


(provide 'core-package)
;;; core-package.el ends here
