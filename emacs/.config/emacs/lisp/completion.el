;;; Ivy/Counsel/Swiper --- (Enhanced Search)
;;; some UI enhenced config
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; company mode
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ; Show immediately

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'completion)
