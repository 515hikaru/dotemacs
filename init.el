;;; package --- @515hikaru emacs initialization

;; Copyright (C) 2019 by Takahiro KOJIMA
;; Author: Takahiro KOJIMA <12kojima.takahiro@gmail.com>
;; URL: https://github.com/515hikaru/dotemacs
;; Version: 0.0.1
;; LICENSE: MIT

;;; Commentary:
;; This package provides Emacs environment for @515hikaru

;;; Code:
;; package el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
;;; use use-package
(require 'use-package)
;;; theme
(use-package atom-one-dark-theme)
;;; font
(let* ((size 15)
       (asciifont "Ricty Diminished Discord")
       (jpfont "Ricty Diminished Discord")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
;; no backup
(setq make-backup-files nil)
(setq make-backup-files nil)
;;; exec-path-from-shell
(when (memq window-system '(mac ns x))
	    (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))
;;; lsp-mode
(use-package lsp-mode
  :commands lsp)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;;; company
(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))
;;; flycheck
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))
;;; magit
(use-package magit
  :bind ("C-x m" . magit-status)
  )
;;; junk file
(use-package open-junk-file
  :config (setq open-junk-file-format "~/memo/%Y-%m-%d-%H%M%S.")
  :bind ("C-c j" . open-junk-file)
  )
;; python
(use-package python-mode
  :defer t
  :commands python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))
(use-package conda
  :init
  (custom-set-variables '(conda-anaconda-home "~/miniconda3"))
  :config
  (conda-env-autoactivate-mode t))
;;; elm environment
(use-package elm-mode
  :config
  (setq elm-format-on-save t)
  (setq elm-format-elm-version 0.19)
  )
(use-package flycheck-elm
  :init
  (eval-after-load 'flycHeck
     '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  )
;; golang environment
(use-package go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :init
  (add-hook 'go-mode-hook #'lsp)
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (add-hook 'before-save-hook 'lsp-format-buffer))
;; rust environment
(use-package rust-mode
  :config
  (setq rust-format-on-save t))
(use-package company-racer
  :defer t
  :init
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq company-tooltip-align-annotations t)
  :bind (:map rust-mode-map
            ("TAB" . #'company-indent-or-complete-common)))
(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
;;; user function
(defun open-memo-dir()
  "Open memo file directory with dired."
  (interactive)
  (dired "~/memo"))
;;; auto config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "~/miniconda3")
 '(package-selected-packages
   (quote
    (ein go-mode yasnippet lsp-ui python-mode company-lsp lsp-mode markdown-mode racer flycheck-rust exec-path-from-shell company-racer rust-mode magit open-junk-file flycheck-elm flycheck company use-package atom-one-dark-theme org-plus-contrib elm-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
