;; Setup package.el repositories
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package emacs
  :init
  ;; Open empty file on startup
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t)
  :bind
  ("M-_" . 'undo-redo)
  :config
  ;; Hide toolbar
  (tool-bar-mode -1)
  :custom
  ;; Enable fullscreen mode on macOS
  (ns-use-native-fullscreen nil))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package timu-caribbean-theme
  :ensure t
  :config
  (load-theme 'timu-caribbean t))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package ag
  :ensure t
  :bind (("C-c s" . ag-project))
  :config
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-reuse-buffers t))

(use-package js2-mode
  :mode ("\\.js\\'" . js-mode)
  :hook (js2-mode . lsp)
  :config
  (setq js2-basic-offset 2))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :hook (json-mode . lsp)
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :hook (web-mode . lsp)
  :mode ("\\.vue\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package lsp-mode
  :commands lsp)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(graphql-mode ag company lsp-mode helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
