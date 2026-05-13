;;; core-package.el --- Custom package setup -*- lexical-binding: t; -*-

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-refresh-contents-if-time-critical t)

(unless (package-installed-p 'use-package)
  (message "use-package not found. Run M-x package-refresh-contents, then M-x package-install use-package"))

(defun ensure-package (pkg)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; doome theme and modelines
(ensure-package 'doom-themes)
(ensure-package 'doom-modeline)

;; fira-code font
(ensure-package 'fira-code-mode)

;; ONLY treemacs – no icon packages
(ensure-package 'treemacs)

(provide 'core-package)
;;; core-package.el ends here
