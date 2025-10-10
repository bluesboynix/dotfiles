;;; gcc-dev.el --- GCC/C/C++ development configuration for Emacs

(eval-and-compile
  (when (fboundp 'package-initialize)
    (package-initialize)))

;; ----- cc-mode defaults -----
(defun gcc-dev--cc-style-setup ()
  "Set sane defaults for editing C/C++/Obj-C files."
  (c-set-style "linux")
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil
        comment-style 'extra)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (subword-mode 1))

;; ----- Minor mode -----
(define-minor-mode gcc-dev-mode
  "Minor mode for GCC C/C++ development."
  :lighter " GCCDev"
  :group 'gcc-dev
  (if gcc-dev-mode
      (progn
        ;; company
        (when (fboundp 'company-mode)
          (company-mode 1))
        ;; flycheck with GCC
        (when (boundp 'flycheck-mode)
          (flycheck-mode 1)))
    ;; disabling
    (when (fboundp 'company-mode) (company-mode -1))))

;; ----- Flycheck: GCC checker -----
(when (require 'flycheck nil t)
  ;; prefer GCC
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-clang-tidy))
  (add-to-list 'flycheck-checkers 'c/c++-gcc))

;; ----- Formatting -----
(defun gcc-dev-format-buffer ()
  "Format buffer using astyle if available."
  (interactive)
  (when (and buffer-file-name
             (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode)))
    (if (executable-find "astyle")
        (shell-command-to-string (format "astyle --style=linux %s" buffer-file-name))
      (message "astyle not found"))))

(defvar gcc-dev--format-on-save nil
  "Non-nil if we format on save in gcc-dev-mode buffers.")
(defun gcc-dev-toggle-format-on-save ()
  "Toggle format-on-save using astyle."
  (interactive)
  (if gcc-dev--format-on-save
      (progn
        (remove-hook 'before-save-hook #'gcc-dev-format-buffer t)
        (setq gcc-dev--format-on-save nil)
        (message "format-on-save disabled"))
    (add-hook 'before-save-hook #'gcc-dev-format-buffer nil t)
    (setq gcc-dev--format-on-save t)
    (message "format-on-save enabled")))

;; ----- Compile command helper -----
(defun gcc-dev-set-compile-command-for-project ()
  "Set `compile-command' using Makefile, CMake, or default GCC build."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                   default-directory))
         (default-cmd
          (cond
           ((file-exists-p (expand-file-name "build.ninja" root))
            (format "ninja -C %s" root))
           ((file-exists-p (expand-file-name "CMakeLists.txt" root))
            (format "cmake --build %s -- -j$(nproc)" (expand-file-name "build" root)))
           ((file-exists-p (expand-file-name "Makefile" root))
            (format "make -C %s -j$(nproc)" root))
           (t (format "gcc %s -o %s.out && ./%s.out"
                      (buffer-file-name)
                      (file-name-base (buffer-file-name))
                      (file-name-base (buffer-file-name)))))))
    (setq-local compile-command default-cmd)
    (message "compile-command set: %s" compile-command)))

;; ----- Hook setup -----
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'objc-mode)
              (gcc-dev-mode 1)
              (local-set-key (kbd "C-c C-c") 'compile)
              (gcc-dev-set-compile-command-for-project))))

(provide 'gcc-dev)
;;; gcc-dev.el ends here
