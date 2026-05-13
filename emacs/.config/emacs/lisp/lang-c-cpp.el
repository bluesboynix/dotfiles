;;; lang-c-cpp.el --- C/C++ configuration (LSP, navigation, compilation) -*- lexical-binding: t; -*-

;; Step-by-step, as needed: C and C++ first.
;; Assumes clangd is installed and in PATH.
;; For Emacs 29+ (Eglot built-in).

;; -------------------------------------------------------------------
;; 1. Basic modes and hooks
;; -------------------------------------------------------------------
(add-hook 'c-mode-hook #'my-c-cpp-common-setup)
(add-hook 'c++-mode-hook #'my-c-cpp-common-setup)

(defun my-c-cpp-common-setup ()
  "Common setup for C and C++ buffers."
  ;; Enable LSP (Eglot) – but only if clangd is available
  (unless (eglot--managed-p)
    (eglot-ensure))
  ;; Optional: better indentation
  (setq-local c-basic-offset 4)
  ;; Enable Flymake integration (Eglot provides diagnostics)
  (flymake-mode +1)
  ;; Local keybindings
  (my-c-cpp-keybindings))

;; -------------------------------------------------------------------
;; 2. Eglot configuration for clangd
;; -------------------------------------------------------------------
(with-eval-after-load 'eglot
  ;; Tell Eglot which LSP server to use for C/C++
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) .
                 ("clangd" "-j=4" "--background-index" "--clang-tidy" "--completion-style=detailed")))
  ;; Optional: hide clangd's stderr chatter
  (setq eglot-report-progress nil))

;; -------------------------------------------------------------------
;; 3. Helper: switch between header and source
;; -------------------------------------------------------------------
(defun my-c-cpp-switch-header-source ()
  "Switch between .c/.cpp and corresponding .h/.hpp file."
  (interactive)
  (let* ((file (buffer-file-name))
         (ext (file-name-extension file))
         (base (file-name-sans-extension file)))
    (cond
     ((member ext '("c" "cpp" "cc" "cxx"))
      (let ((header (concat base ".h")))
        (if (file-exists-p header)
            (find-file header)
          (let ((hpp (concat base ".hpp")))
            (if (file-exists-p hpp)
                (find-file hpp)
              (message "No header file found"))))))
     ((member ext '("h" "hpp"))
      (let ((src (concat base ".c")))
        (if (file-exists-p src)
            (find-file src)
          (let ((cpp (concat base ".cpp")))
            (if (file-exists-p cpp)
                (find-file cpp)
              (message "No source file found"))))))
     (t (message "Not a C/C++ file")))))

;; -------------------------------------------------------------------
;; 4. Compilation (uses project root if available)
;; -------------------------------------------------------------------
(defun my-c-cpp-compile ()
  "Run make or a custom compile command in the project root."
  (interactive)
  (let ((default-directory (or (project-root (project-current)) default-directory)))
    (compile "make -k")))

;; -------------------------------------------------------------------
;; 5. Keybindings (only in C/C++ buffers)
;; -------------------------------------------------------------------
(defun my-c-cpp-keybindings ()
  "Local keymap for C/C++ modes."
  (local-set-key (kbd "C-c C-s") #'my-c-cpp-switch-header-source)
  (local-set-key (kbd "C-c C-c") #'my-c-cpp-compile)
  (local-set-key (kbd "C-c C-r") #'eglot-rename)
  (local-set-key (kbd "C-c C-f") #'eglot-format)
  (local-set-key (kbd "M-.")     #'xref-find-definitions)
  (local-set-key (kbd "M-,")     #'xref-pop-marker-stack))

;; -------------------------------------------------------------------
;; 6. Optional: company-mode completion (if you use company)
;; -------------------------------------------------------------------
(with-eval-after-load 'company
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode))

;; -------------------------------------------------------------------
;; 7. Optional: projectile integration (if used)
;; -------------------------------------------------------------------
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "compile_commands.json")
  (add-to-list 'projectile-project-root-files-bottom-up ".clangd"))

(provide 'lang-c-cpp)
;;; lang-c-cpp.el ends here
