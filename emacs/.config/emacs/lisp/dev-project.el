;;; dev-project.el --- Project management layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Enhances built-in project.el
;; Smart project detection + better compilation workflow.
;; No UI dependencies.

;;; Code:

;; ============================================================
;; Built-in project.el
;; ============================================================

(use-package project
  :config

  ;; Use ripgrep if available
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-vc-dir "VC Dir")
          (project-compile "Compile")))

  ;; Faster file enumeration
  (setq project-vc-extra-root-markers
        '("compile_commands.json"
          "CMakeLists.txt"
          "go.mod"
          "Cargo.toml"
          "package.json")))

;; ============================================================
;; Smart Compilation
;; ============================================================

(defun dev-project-guess-compile-command ()
  "Guess a reasonable compile command based on project files."
  (cond
   ;; CMake
   ((locate-dominating-file default-directory "CMakeLists.txt")
    "cmake --build build")

   ;; Makefile
   ((locate-dominating-file default-directory "Makefile")
    "make -k")

   ;; Go
   ((locate-dominating-file default-directory "go.mod")
    "go build ./...")

   ;; Rust
   ((locate-dominating-file default-directory "Cargo.toml")
    "cargo build")

   ;; Node
   ((locate-dominating-file default-directory "package.json")
    "npm run build")

   (t compile-command)))

(defun dev-project-set-compile-command ()
  "Auto-set `compile-command` locally."
  (setq-local compile-command
              (dev-project-guess-compile-command)))

(add-hook 'prog-mode-hook #'dev-project-set-compile-command)

;; ============================================================
;; Convenience Keybindings
;; ============================================================

(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p s") #'project-find-regexp)
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p c") #'project-compile)

(provide 'dev-project)
;;; dev-project.el ends here
