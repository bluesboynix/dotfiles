;;; core-package.el --- Custom package setup -*- lexical-binding: t; -*-

;; Do NOT (require 'package) here – it's already built-in

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-refresh-contents-if-time-critical t)

(unless (package-installed-p 'use-package)
  (message "use-package not found. Run M-x package-refresh-contents, then M-x package-install use-package"))

(provide 'core-package)
;;; core-package.el ends here
