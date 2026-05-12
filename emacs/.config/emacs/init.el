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

(message "init.el loaded all modules successfully.")

