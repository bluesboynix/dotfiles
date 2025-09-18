;; ====================
;; Package Management
;; ====================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;;(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)  ; Debugging


;; Package to read path from shell
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))


(provide 'packages)
;;; packages.el ends here
