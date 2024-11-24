;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package emacs
  :init
  ;; Open empty file on startup
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t)
  :bind
  ;; Bind redo to M-_
  ("M-_" . 'undo-redo)
  :config
  ;; Hide toolbar
  (tool-bar-mode -1)
  :custom
  ;; Enable fullscreen mode on macOS
  (ns-use-native-fullscreen nil))

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-items))
  (dashboard-setup-startup-hook))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package ag
  :bind (("C-c s" . ag-project))
  :config
  (setq ag-highlight-search t))

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

(use-package vue-mode
  :hook (vue-mode . lsp)
  :mode ("\\.vue\\'" . vue-mode)
  :config
  (setq mmm-submode-decoration-level 0))

(use-package graphql-mode
  :hook (graphql-mode . lsp)
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package lsp-mode
  :commands lsp)

(use-package corfu
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode))
