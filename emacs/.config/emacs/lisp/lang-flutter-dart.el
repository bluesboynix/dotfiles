;;; lang-flutter-dart.el --- Flutter/Dart development configuration for Emacs

;;; Commentary:
;; Configuration for Flutter and Dart development in Emacs using use-package

;;; Code:

;; ============================================================================
;; Dart Mode
;; ============================================================================

(use-package dart-mode
  :ensure t
  :mode ("\\.dart\\'" . dart-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
  (add-to-list 'auto-mode-alist '("pubspec\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.arb\\'" . json-mode))
  :config
  (setq dart-format-on-save t)
  (setq dart-expand-to-brace t)
  (setq dart-indent-offset 2)
  
  ;; Enhanced syntax highlighting
  (font-lock-add-keywords
   'dart-mode
   '(("\\<\\(Widget\\|StatefulWidget\\|StatelessWidget\\|BuildContext\\)\\>"
      1 font-lock-type-face)))
  
  (defface dart-font-lock-flutter-widget
    '((t :inherit font-lock-type-face :weight bold))
    "Face for Flutter widget names."
    :group 'dart-mode)
  
  (font-lock-add-keywords
   'dart-mode
   '(("\\<\\(MaterialApp\\|Scaffold\\|Container\\|Column\\|Row\\|Text\\|Icon\\|AppBar\\|FloatingActionButton\\)\\>"
      1 'dart-font-lock-flutter-widget))))

;; ============================================================================
;; LSP Mode
;; ============================================================================

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-headerline-breadcrumb-enable t)
  
  ;; Enable Flycheck integration
  (setq lsp-diagnostics-provider :flycheck)
  
  (lsp-enable-which-key-integration t))

;; ============================================================================
;; LSP Dart
;; ============================================================================

(use-package lsp-dart
  :ensure t
  :after (dart-mode lsp-mode)
  :hook (dart-mode . lsp-deferred)
  :init
  (add-hook 'dart-mode-hook #'lsp-deferred)
  :config
  ;; SDK paths
  (setq lsp-dart-sdk-dir (or (getenv "FLUTTER_ROOT") 
                             (getenv "DART_SDK")
                             "/usr/lib/dart"))
  
  (setq lsp-dart-flutter-sdk-dir (or (getenv "FLUTTER_ROOT")
                                     lsp-dart-sdk-dir))
  
  ;; Analysis server
  (setq lsp-dart-line-length 80)
  (setq lsp-dart-closing-labels t)
  (setq lsp-dart-flutter-widget-guides t)
  (setq lsp-dart-outline t)
  (setq lsp-dart-suggest-from-unimported-libraries t)
  
  ;; Flutter features
  (setq lsp-dart-enable-flutter-daemon t)
  
  ;; Formatting
  (setq lsp-dart-format-on-save t))

;; ============================================================================
;; Company (Completion)
;; ============================================================================

(use-package company
  :ensure t
  :hook (dart-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 15)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  
  (add-to-list 'company-backends 'company-capf)
  
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; ============================================================================
;; Flycheck
;; ============================================================================

(use-package flycheck
  :ensure t
  :hook (dart-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 0.8))

;; ============================================================================
;; Flutter Commands
;; ============================================================================

(defun my/flutter-run ()
  "Run Flutter app."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter run")))

(defun my/flutter-hot-reload ()
  "Hot reload Flutter app."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (shell-command "echo 'r' | flutter -d $(flutter devices | awk 'NR==2 {print $2}')")))

(defun my/flutter-hot-restart ()
  "Hot restart Flutter app."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (shell-command "echo 'R' | flutter -d $(flutter devices | awk 'NR==2 {print $2}')")))

(defun my/flutter-pub-get ()
  "Run 'flutter pub get'."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter pub get")))

(defun my/flutter-clean ()
  "Clean Flutter project."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter clean")))

(defun my/flutter-doctor ()
  "Run Flutter doctor."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter doctor -v")))

(defun my/flutter-test-file ()
  "Run tests in current file."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory))
        (file (buffer-file-name)))
    (when file
      (compile (format "flutter test %s" file)))))

(defun my/flutter-test-all ()
  "Run all tests."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter test")))

(defun my/flutter-build-apk ()
  "Build APK."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter build apk --release")))

;; ============================================================================
;; Keybindings
;; ============================================================================

(defun my/dart-setup-keybindings ()
  "Setup keybindings for Dart/Flutter."
  (local-set-key (kbd "C-c f r") 'my/flutter-run)
  (local-set-key (kbd "C-c f R") 'my/flutter-hot-restart)
  (local-set-key (kbd "C-c f h") 'my/flutter-hot-reload)
  (local-set-key (kbd "C-c f g") 'my/flutter-pub-get)
  (local-set-key (kbd "C-c f c") 'my/flutter-clean)
  (local-set-key (kbd "C-c f d") 'my/flutter-doctor)
  (local-set-key (kbd "C-c f t") 'my/flutter-test-file)
  (local-set-key (kbd "C-c f T") 'my/flutter-test-all)
  (local-set-key (kbd "C-c f f") 'lsp-format-buffer)
  (local-set-key (kbd "C-c f a") 'lsp-execute-code-action)
  
  ;; Debug helpers
  (local-set-key (kbd "C-c d p") 'my/dart-insert-print-statement))

(add-hook 'dart-mode-hook 'my/dart-setup-keybindings)

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun my/dart-insert-print-statement ()
  "Insert debug print statement."
  (interactive)
  (insert "print('DEBUG: ');")
  (backward-char 2))

(defun my/dart-extract-widget ()
  "Extract selected code as widget."
  (interactive)
  (when (use-region-p)
    (let ((code (buffer-substring (region-beginning) (region-end))))
      (kill-region (region-beginning) (region-end))
      (insert "\n// TODO: Implement extracted widget\n")
      (save-excursion
        (goto-char (point-max))
        (insert "\n\nclass ExtractedWidget extends StatelessWidget {\n")
        (insert "  const ExtractedWidget({Key? key}) : super(key: key);\n\n")
        (insert "  @override\n")
        (insert "  Widget build(BuildContext context) {\n")
        (insert "    return " code ";\n")
        (insert "  }\n")
        (insert "}\n")))))

(defun my/verify-flutter-setup ()
  "Verify Flutter/Dart setup."
  (interactive)
  (message "Checking Flutter/Dart setup...")
  (if (and (getenv "FLUTTER_ROOT")
           (executable-find "flutter")
           (executable-find "dart"))
      (message "✓ Flutter/Dart setup verified!")
    (message "⚠ Some issues found. Check environment variables."))
  (message "Dart SDK: %s" lsp-dart-sdk-dir)
  (message "Flutter SDK: %s" lsp-dart-flutter-sdk-dir))

;; ============================================================================
;; Yasnippet (Optional)
;; ============================================================================

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (dart-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; ============================================================================
;; Which-Key
;; ============================================================================

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "C-c f" "flutter"
    "C-c d" "dart debug"))

;; ============================================================================
;; Projectile Integration
;; ============================================================================

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml"))

;; ============================================================================
;; Modeline Indicator
;; ============================================================================

(defvar-local my/dart-flutter-project nil)

(defun my/dart-update-mode-line ()
  (setq my/dart-flutter-project
        (and (buffer-file-name)
             (locate-dominating-file (buffer-file-name) "pubspec.yaml"))))

(add-hook 'dart-mode-hook 'my/dart-update-mode-line)

(let ((flutter-indicator '(:eval (when my/dart-flutter-project " 🐦"))))
  (add-to-list 'mode-line-misc-info flutter-indicator))

;; ============================================================================
;; Format on Save
;; ============================================================================

(defun my/dart-format-on-save ()
  (when (and (boundp 'lsp-mode) lsp-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'my/dart-format-on-save nil t)

;; ============================================================================
;; Quick Start Commands
;; ============================================================================

(defun my/new-flutter-project ()
  "Create new Flutter project."
  (interactive)
  (let ((project-name (read-string "Project name: ")))
    (shell-command (format "flutter create %s" project-name))
    (find-file (concat project-name "/lib/main.dart"))))

(defun my/open-flutter-project ()
  "Open Flutter project."
  (interactive)
  (let ((project-dir (read-directory-name "Flutter project: ")))
    (find-file (concat project-dir "/lib/main.dart"))))

;; ============================================================================
;; Provide
;; ============================================================================

(provide 'lang-flutter-dart)

;;; lang-flutter-dart.el ends here
