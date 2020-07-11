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
(package-install 'use-package)
(require 'use-package)
(add-to-list 'load-path "~/.emacs.d/my-elisp/")
;;; start up
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(tool-bar-mode 0)
(menu-bar-mode 0)
;;; count chars
(add-to-list 'global-mode-string '(" %i"))
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
;;; IME
;;; mozc
(use-package mozc
  :ensure t
  :config
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (prefer-coding-system 'utf-8))
;;; theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(bar window-number buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))
(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode 1)
  (setq rainbow-delimiters-outermost-only-face-count 1))
;;; font
(let* ((size 15)
       (asciifont "Ricty")
       (jpfont "Ricty")
       (h (* size 18))
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
;;;
(run-with-idle-timer 30 t
                     '(lambda ()
                        (with-suppressed-message (recentf-save-list))))
;;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
        (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))
;;; magit
(use-package magit
  :ensure t
  :bind ("C-x m" . magit-status))
;;; junk file
(use-package open-junk-file
  :ensure t
  :config (setq open-junk-file-format (concat (getenv "HOME") "/memo/%Y-%m-%d-%H%M%S."))
  :bind ("C-c j" . open-junk-file))
(defun edit-init-file()
  "open $HOME/.emacs.d/init.el"
  (interactive)
  (find-file (concat (getenv "HOME") "/.emacs.d/init.el")))
(global-set-key "\C-c\C-e" 'edit-init-file)
(use-package recentf
  :ensure t
  :bind ("C-x C-r" . 'counsel-recentf)
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list)))))
(use-package recentf-ext
  :ensure t)
(use-package counsel
  :ensure t
  :init (ivy-mode 1) ;; デフォルトの入力補完がivyになる
  (counsel-mode 1))
;;; projectile
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (with-eval-after-load 'magit
    (setq magit-repository-directories
        '(;; Directory containing project root directories
          ("~/ghq/src"      . 3))))
    (with-eval-after-load 'projectile
      (when (require 'magit nil t)
      (mapc #'projectile-add-known-project
            (mapcar #'file-name-as-directory (magit-list-repos)))
      ;; Optionally write to persistent `projectile-known-projects-file'
      (projectile-save-known-projects))))
;;; easy-hugo
(use-package easy-hugo
  :ensure t
  :init (setq easy-hugo-basedir "~/ghq/src/github.com/515hikaru/tech-memo/")
        (setq easy-hugo-url "https://tech.515hikaru.net")
        (define-key global-map (kbd "C-c C-h") 'easy-hugo)
        (setq easy-hugo-bloglist
        '(((easy-hugo-basedir . "~/src/github.com/masasam/hugo2/")
           (easy-hugo-url . "http://example2.com")
           (easy-hugo-sshdomain . "myblogdomain")
           (easy-hugo-root . "/home/hugo/"))))
        :bind ("C-c C-h" . easy-hugo))
;;; org-mode
;;; org-agenda
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-agenda-files
      '("~/dev/github.com/515hikaru/org-memo/top.org"
        "~/dev/github.com/515hikaru/org-memo/knowledge.org"
        "~/dev/github.com/515hikaru/org-memo/remind.org"))
;;; org-capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/dev/github.com/515hikaru/org-memo/remind.org" "■Capture") "* REMIND %? (wrote on %U)")
        ("k" "Knowledge" entry (file+headline "~/dev/github.com/515hikaru/org-memo/knowledge.org" "TOP") "* %?\n  # Wrote on %U")
        ("n" "News" entry (file+headline "~/dev/github.com/515hikaru/org-memo/news.org" "NEWS") "* %?\n  # Wrote on %U")
        ("p" "Technology" entry (file+headline "~/dev/github.com/515hikaru/org-memo/techs.org" "Techs") "* %?\n  # Wrote on %U")
        ("w" "Work" entry (file+headline "~/dev/github.com/515hikaru/org-memo/work.org" "Work") "* %?\n  # Wrote on %U")))
;;; TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "REMIND(r)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELED(c)")))
;;; ox-hugo
(use-package ox-hugo
  :ensure t
  :after ox)
;;; truncate
(global-set-key "\C-c$" 'toggle-truncate-lines)
;; pnovel mode
(define-generic-mode pnovel-mode
  '("%")
  ;; キーワードの指定
  '("newline" "newpage")
  ;; もうちょっと難しいキーワードの指定
  '(("# .*" . font-lock-warning-face)
    ("`.*`" . font-lock-doc-face))
  nil nil
  "Major mode for pnovel")
(add-to-list 'auto-mode-alist '("\\.pnovel\\'" . pnovel-mode))
;;; auto config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-lsp-enable-snippet nil)
 '(conda-anaconda-home "~/miniconda3")
 '(lsp-clients-go-func-snippet-enabled nil)
 '(package-selected-packages
   (quote
    (ox-hugo mozc prettier-js auto-virtualenvwrapper poetry easy-hugo doom-modeline all-the-icons doom-themes projectile yasnippet-snippets solidity-flycheck recentf-ext elm-mode counsel-ghq counsel ivy writeroom-mode hcl-mode subr-x neotree elixir-mode dockerfile-mode toml-mode yaml-mode julia-repl flycheck-julia julia-mode conda ein go-mode yasnippet lsp-ui python-mode company-lsp lsp-mode markdown-mode racer flycheck-rust exec-path-from-shell company-racer rust-mode magit open-junk-file flycheck-elm company use-package atom-one-dark-theme org-plus-contrib))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
