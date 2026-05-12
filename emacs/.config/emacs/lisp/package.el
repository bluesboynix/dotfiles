;;; package.el --- Minimal package management setup -*- lexical-binding: t; -*-

;; Set up package archives
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Auto-refresh once a week
(setq package-refresh-contents-if-time-critical t)

;; If use-package isn't installed, offer to install it on first load
(unless (package-installed-p 'use-package)
  (message "use-package not found. Run M-x package-refresh-contents, then M-x package-install use-package"))

(provide 'package)
;;; package.el ends here
