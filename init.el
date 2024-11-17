(eval-when-compile
  (require 'use-package))

(use-package emacs
  :init
  ;; Hide toolbar
  (tool-bar-mode -1)
  ;; Open empty file on startup
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config ((helm-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custocm.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
