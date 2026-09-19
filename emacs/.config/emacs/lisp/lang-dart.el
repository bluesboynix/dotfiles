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
;;   - flutter.el for run / hot-reload / hot-restart (Flutter projects)
;;   - compile-based helpers for standalone Dart projects (dart run/test/pub)
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
;; 3. Standalone Dart helpers
;; ---------------------------------------------------------------------------
;;
;; These are for pure-Dart packages (no Flutter).  They use the plain `dart'
;; binary -- even when it comes from the Flutter-bundled SDK, it behaves like
;; a standalone Dart SDK for non-Flutter projects.

(defun my/dart--project-root ()
  "Return the nearest directory containing a pubspec.yaml, or `default-directory'."
  (or (locate-dominating-file default-directory "pubspec.yaml")
      default-directory))

(defun my/dart-run ()
  "Run `dart run' in the current package (the default executable)."
  (interactive)
  (let ((default-directory (my/dart--project-root)))
    (compile "dart run")))

(defun my/dart-run-file ()
  "Run the current .dart file with `dart run'."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (compile (format "dart run %s" (shell-quote-argument buffer-file-name))))

(defun my/dart-test ()
  "Run `dart test' for the current package."
  (interactive)
  (let ((default-directory (my/dart--project-root)))
    (compile "dart test")))

(defun my/dart-pub-get ()
  "Run `dart pub get' in the current package."
  (interactive)
  (let ((default-directory (my/dart--project-root)))
    (compile "dart pub get")))

(defun my/dart-analyze ()
  "Run `dart analyze' on the current package."
  (interactive)
  (let ((default-directory (my/dart--project-root)))
    (compile "dart analyze")))

(defun my/dart-format-buffer ()
  "Run `dart format' on the current buffer's file."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (compile (format "dart format %s" (shell-quote-argument buffer-file-name))))

;; ---------------------------------------------------------------------------
;; 4. Eglot (LSP)
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
;; 5. Flutter + keybindings
;; ---------------------------------------------------------------------------

(use-package flutter
  :ensure t
  :after dart-mode
  :custom
  (flutter-sdk-path my/flutter-root))

(with-eval-after-load 'dart-mode
  (define-key dart-mode-map (kbd "C-c C-f") #'flutter-run-or-hot-reload)
  (define-key dart-mode-map (kbd "C-c C-r") #'flutter-hot-reload)
  (define-key dart-mode-map (kbd "C-c C-R") #'flutter-hot-restart)

  ;; Standalone Dart under a C-c d prefix
  (define-key dart-mode-map (kbd "C-c d r") #'my/dart-run)
  (define-key dart-mode-map (kbd "C-c d f") #'my/dart-run-file)
  (define-key dart-mode-map (kbd "C-c d t") #'my/dart-test)
  (define-key dart-mode-map (kbd "C-c d p") #'my/dart-pub-get)
  (define-key dart-mode-map (kbd "C-c d a") #'my/dart-analyze)
  (define-key dart-mode-map (kbd "C-c d F") #'my/dart-format-buffer))

(provide 'lang-dart)
;;; lang-dart.el ends here
