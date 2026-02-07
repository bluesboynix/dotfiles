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
  ;; Add to auto-mode-alist
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
  (add-to-list 'auto-mode-alist '("pubspec\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.arb\\'" . json-mode))
  :config
  ;; Dart mode configuration
  (setq dart-format-on-save t)
  (setq dart-expand-to-brace t)
  (setq dart-indent-offset 2)
  
  ;; Syntax highlighting enhancements
  (font-lock-add-keywords
   'dart-mode
   '(("\\<\\(Widget\\|StatefulWidget\\|StatelessWidget\\|BuildContext\\)\\>"
      1 font-lock-type-face)))
  
  ;; Custom faces for Flutter widgets
  (defface dart-font-lock-flutter-widget
    '((t :inherit font-lock-type-face :weight bold))
    "Face for Flutter widget names."
    :group 'dart-mode)
  
  (font-lock-add-keywords
   'dart-mode
   '(("\\<\\(MaterialApp\\|Scaffold\\|Container\\|Column\\|Row\\|Text\\|Icon\\|AppBar\\|FloatingActionButton\\)\\>"
      1 'dart-font-lock-flutter-widget))))

;; ============================================================================
;; LSP Mode (Base)
;; ============================================================================

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  ;; Set LSP key prefix
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; General LSP settings
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil) ; Set to t for debugging
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-enable-text-document-color t)
  
  ;; Performance optimizations
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-file-watchers t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  
  ;; Enable which-key integration
  (lsp-enable-which-key-integration t)
  
  ;; Flycheck integration
  (require 'flycheck)
  (setq lsp-diagnostics-provider :flycheck))

;; ============================================================================
;; LSP Dart
;; ============================================================================

(use-package lsp-dart
  :ensure t
  :after (dart-mode lsp-mode)
  :hook (dart-mode . lsp-deferred)
  :init
  ;; Ensure Dart mode triggers LSP
  (add-hook 'dart-mode-hook #'lsp-deferred)
  :config
  ;; Dart SDK configuration
  (setq lsp-dart-sdk-dir (or (getenv "FLUTTER_ROOT") 
                             (getenv "DART_SDK")
                             "/usr/lib/dart"))
  
  ;; Flutter SDK configuration
  (setq lsp-dart-flutter-sdk-dir (or (getenv "FLUTTER_ROOT")
                                     lsp-dart-sdk-dir))
  
  ;; Analysis server configuration
  (setq lsp-dart-line-length 80)
  (setq lsp-dart-analyzer-instrumentation-log-file nil)
  (setq lsp-dart-flutter-daemon-script-name "flutter")
  
  ;; Analysis features
  (setq lsp-dart-analysis-sdk-recommendations nil)
  (setq lsp-dart-closing-labels t)
  (setq lsp-dart-flutter-widget-guides t)
  (setq lsp-dart-outline t)
  (setq lsp-dart-suggest-from-unimported-libraries t)
  (setq lsp-dart-allow-opening-outside-workspace t)
  
  ;; Enable Flutter daemon for hot reload
  (setq lsp-dart-enable-flutter-daemon t)
  
  ;; Formatting
  (setq lsp-dart-format-on-save t)
  (setq lsp-dart-format-on-type t))

;; ============================================================================
;; Company (Completion)
;; ============================================================================

(use-package company
  :ensure t
  :hook (dart-mode . company-mode)
  :config
  ;; General company settings
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 15)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  
  ;; Backend configuration
  (add-to-list 'company-backends 'company-capf)
  
  ;; Keybindings
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; ============================================================================
;; Flycheck (Syntax Checking - via LSP)
;; ============================================================================

(use-package flycheck
  :ensure t
  :hook (dart-mode . flycheck-mode)
  :config
  ;; General flycheck settings
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 0.8)
  
  ;; Disable certain checkers that might conflict with LSP
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; ============================================================================
;; Flutter Tools Integration
;; ============================================================================

(defun my/flutter-run ()
  "Run Flutter app in current directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter run")))

(defun my/flutter-hot-reload ()
  "Send hot reload command to Flutter."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (shell-command "echo 'R' | flutter -d $(flutter devices | awk 'NR==2 {print $2}')")))

(defun my/flutter-hot-restart ()
  "Send hot restart command to Flutter."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (shell-command "echo 'r' | flutter -d $(flutter devices | awk 'NR==2 {print $2}')")))

(defun my/flutter-packages-get ()
  "Run 'flutter packages get'."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter pub get")))

(defun my/flutter-build-apk ()
  "Build APK."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter build apk --release")))

(defun my/flutter-build-ios ()
  "Build iOS."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter build ios --release")))

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

(defun my/flutter-test-current-file ()
  "Run tests in current Dart file."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory))
        (file (buffer-file-name)))
    (when file
      (compile (format "flutter test %s" file)))))

(defun my/flutter-test-all ()
  "Run all tests in project."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "flutter test")))

;; ============================================================================
;; Projectile Integration
;; ============================================================================

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "pubspec.yaml")
  (add-to-list 'projectile-project-root-files ".dart_tool")
  (add-to-list 'projectile-project-root-files ".flutter-plugins")
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml"))

;; ============================================================================
;; Keybindings
;; ============================================================================

(defun my/dart-setup-keybindings ()
  "Setup keybindings for Dart/Flutter development."
  (local-set-key (kbd "C-c f r") 'my/flutter-run)
  (local-set-key (kbd "C-c f R") 'my/flutter-hot-restart)
  (local-set-key (kbd "C-c f h") 'my/flutter-hot-reload)
  (local-set-key (kbd "C-c f g") 'my/flutter-packages-get)
  (local-set-key (kbd "C-c f c") 'my/flutter-clean)
  (local-set-key (kbd "C-c f b") 'my/flutter-build-apk)
  (local-set-key (kbd "C-c f i") 'my/flutter-build-ios)
  (local-set-key (kbd "C-c f d") 'my/flutter-doctor)
  (local-set-key (kbd "C-c f t") 'my/flutter-test-current-file)
  (local-set-key (kbd "C-c f T") 'my/flutter-test-all)
  (local-set-key (kbd "C-c f f") 'lsp-format-buffer)
  (local-set-key (kbd "C-c f a") 'lsp-execute-code-action)
  
  ;; Debug helpers
  (local-set-key (kbd "C-c d p") 'my/dart-insert-print-statement)
  (local-set-key (kbd "C-c d w") 'my/dart-extract-widget))

(add-hook 'dart-mode-hook 'my/dart-setup-keybindings)

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun my/dart-insert-print-statement ()
  "Insert a debug print statement at point."
  (interactive)
  (insert "print('DEBUG: ');")
  (backward-char 2))

(defun my/dart-extract-widget ()
  "Extract selected code as a new widget."
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
  "Verify Flutter/Dart environment setup."
  (interactive)
  (message "Checking Flutter/Dart setup...")
  
  (let ((issues nil))
    (unless (getenv "FLUTTER_ROOT")
      (setq issues (cons "✗ FLUTTER_ROOT environment variable not set" issues))
      (message "✗ FLUTTER_ROOT environment variable not set"))
    
    (unless (executable-find "flutter")
      (setq issues (cons "✗ Flutter executable not found in PATH" issues))
      (message "✗ Flutter executable not found in PATH"))
    
    (unless (executable-find "dart")
      (setq issues (cons "✗ Dart executable not found in PATH" issues))
      (message "✗ Dart executable not found in PATH"))
    
    (if issues
        (progn
          (message "Setup issues found. Please check:")
          (dolist (issue issues)
            (message "  %s" issue)))
      (message "✓ Flutter/Dart setup verified successfully!")))
  
  (message "Dart SDK path: %s" lsp-dart-sdk-dir)
  (message "Flutter SDK path: %s" lsp-dart-flutter-sdk-dir))

;; ============================================================================
;; Yasnippet Integration (Optional)
;; ============================================================================

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (dart-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/dart-mode" user-emacs-directory) t))

;; ============================================================================
;; Tree-sitter Support (Conditional - Only for Emacs 29+)
;; ============================================================================

(defun my/setup-dart-tree-sitter ()
  "Setup tree-sitter for Dart if available."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (require 'treesit nil t))
    (message "Tree-sitter available, checking for Dart grammar...")
    (when (treesit-language-available-p 'dart)
      (message "Dart tree-sitter grammar available, enabling...")
      ;; Define dart-ts-mode if not already defined
      (unless (fboundp 'dart-ts-mode)
        (define-derived-mode dart-ts-mode prog-mode "Dart[TS]"
          "Major mode for editing Dart files with tree-sitter."
          :group 'dart
          (when (treesit-ready-p 'dart)
            (treesit-parser-create 'dart)
            (setq-local treesit-font-lock-settings
                        (treesit-font-lock-rules
                         :language 'dart
                         :feature 'comment
                         '((comment) @font-lock-comment-face)
                         
                         :language 'dart
                         :feature 'string
                         '((string_literal) @font-lock-string-face)
                         
                         :language 'dart
                         :feature 'keyword
                         '([(async) (await) (break) (case) (catch) (class)
                            (const) (continue) (default) (do) (else) (enum)
                            (extends) (final) (finally) (for) (if) (in)
                            (is) (new) (null) (return) (super) (switch)
                            (this) (throw) (try) (var) (void) (while)
                            (with) (yield)] @font-lock-keyword-face)
                         
                         :language 'dart
                         :feature 'type
                         '((type_identifier) @font-lock-type-face)
                         
                         :language 'dart
                         :feature 'function
                         '((function_signature
                            name: (identifier) @font-lock-function-name-face))
                         
                         :language 'dart
                         :feature 'variable
                         '((identifier) @font-lock-variable-name-face)))
            
            (setq-local treesit-font-lock-feature-list
                        '((comment string)
                          (keyword type)
                          (function variable)))
            
            (treesit-major-mode-setup))))
      
      ;; Add mode mapping
      (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-ts-mode))
      
      ;; Hook up LSP for dart-ts-mode
      (add-hook 'dart-ts-mode-hook #'lsp-deferred)
      (add-hook 'dart-ts-mode-hook 'my/dart-setup-keybindings)
      (add-hook 'dart-ts-mode-hook 'my/dart-update-mode-line))))

;; Run tree-sitter setup on initialization
(add-hook 'emacs-startup-hook 'my/setup-dart-tree-sitter)

;; ============================================================================
;; Which-Key Integration
;; ============================================================================

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  
  ;; Add descriptions for our keybindings
  (which-key-add-key-based-replacements
    "C-c f" "flutter"
    "C-c d" "dart debug"))

;; ============================================================================
;; Treemacs Integration (Optional)
;; ============================================================================

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; ============================================================================
;; Modeline Integration
;; ============================================================================

(defvar-local my/dart-flutter-project nil
  "Whether current project is a Flutter project.")

(defun my/dart-update-mode-line ()
  "Update mode line for Dart/Flutter projects."
  (setq my/dart-flutter-project
        (and (buffer-file-name)
             (locate-dominating-file (buffer-file-name) "pubspec.yaml"))))

(add-hook 'dart-mode-hook 'my/dart-update-mode-line)

;; Add Flutter indicator to mode line
(let ((flutter-indicator '(:eval (when my/dart-flutter-project " 🐦"))))
  (add-to-list 'mode-line-misc-info flutter-indicator))

;; ============================================================================
;; Format on Save
;; ============================================================================

(defun my/dart-format-on-save ()
  "Format Dart buffer on save if lsp is active."
  (when (and (boundp 'lsp-mode) lsp-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'my/dart-format-on-save nil t)

;; ============================================================================
;; Setup Verification on Load
;; ============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (getenv "FLUTTER_ROOT")
              (message "Flutter environment detected: %s" (getenv "FLUTTER_ROOT")))))

;; ============================================================================
;; Provide
;; ============================================================================

(provide 'lang-flutter-dart)

;;; lang-flutter-dart.el ends here
