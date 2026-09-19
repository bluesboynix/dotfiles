;;; lang-dart.el --- Dart / Flutter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Assumes Flutter is installed at ~/develop/flutter/, i.e.:
;;   ~/develop/flutter/bin/flutter
;;   ~/develop/flutter/bin/dart
;;   ~/develop/flutter/bin/cache/dart-sdk/       (bundled Dart SDK)
;;
;; Provides:
;;   - PATH / FLUTTER_ROOT / DART_SDK setup
;;   - dart-mode
;;   - Eglot talking to `dart language-server'
;;   - flutter.el for run / hot-reload / hot-restart
;;
;; Requires Emacs 29+ (eglot is built-in).

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Paths
;; ---------------------------------------------------------------------------

(defconst my/flutter-root (expand-file-name "~/develop/flutter")
  "Root of the Flutter installation.")

(defconst my/flutter-bin  (expand-file-name "bin" my/flutter-root)
  "Flutter's bin directory (holds `flutter' and `dart' shims).")

(defconst my/dart-sdk     (expand-file-name "bin/cache/dart-sdk" my/flutter-root)
  "Bundled Dart SDK shipped with Flutter.")

(defconst my/dart-sdk-bin (expand-file-name "bin" my/dart-sdk)
  "Bin directory of the bundled Dart SDK.")

;; Make `flutter' and `dart' discoverable by Emacs and by any subprocess
;; (Eglot, flutter.el, compile, etc.).
(dolist (dir (list my/flutter-bin my/dart-sdk-bin))
  (add-to-list 'exec-path dir))
(setenv "PATH"
        (concat my/flutter-bin ":"
                my/dart-sdk-bin ":"
                (or (getenv "PATH") "")))
(setenv "FLUTTER_ROOT" my/flutter-root)
(setenv "DART_SDK"     my/dart-sdk)

;; ---------------------------------------------------------------------------
;; 2. dart-mode
;; ---------------------------------------------------------------------------

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'"
  :custom
  (dart-sdk-path my/dart-sdk)
  (dart-format-on-save t)
  (dart-indent-offset 2)
  :hook
  (dart-mode . (lambda ()
                 (setq-local tab-width 2)
                 (setq-local indent-tabs-mode nil))))

;; ---------------------------------------------------------------------------
;; 3. Eglot (LSP)
;; ---------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :hook (dart-mode . eglot-ensure)
  :custom
  (eglot-autoreconnect t)
  (eglot-sync-connect 1)
  :config
  ;; `dart language-server' does NOT accept `--loglevel' -- passing it
  ;; causes the server to exit immediately (the "-1: server died" error).
  (add-to-list 'eglot-server-programs
               '((dart-mode)
                 . ("dart" "language-server"
                    "--client-id" "emacs.eglot-dart"
                    "--client-version" "1.0"))))

;; ---------------------------------------------------------------------------
;; 4. Flutter
;; ---------------------------------------------------------------------------

(use-package flutter
  :ensure t
  :after dart-mode
  :custom
  (flutter-sdk-path my/flutter-root)
  :bind (:map dart-mode-map
              ("C-c C-f" . flutter-run-or-hot-reload)
              ("C-c C-r" . flutter-hot-reload)
              ("C-c C-R" . flutter-hot-restart)))

(provide 'lang-dart)
;;; lang-dart.el ends here
