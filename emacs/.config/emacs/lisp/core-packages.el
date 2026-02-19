;;; core-packages.el --- Extended package setup -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ("elpa-devel"   . "https://elpa.gnu.org/devel/")))

(setq package-archive-priorities
      '(("gnu"          . 30)
        ("nongnu"       . 25)
        ("org"          . 20)
        ("melpa-stable" . 15)
        ("melpa"        . 10)
        ("elpa-devel"   . 0)))  ;; lowest priority (bleeding edge)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'core-packages)
