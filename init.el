;; setup custom file path
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; setup use-package
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; setup ui
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(set-face-attribute 'default nil :family "Osaka−等幅" :height 140)

;; share clipboard with OS
(setq select-enable-clipboard t)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/zettelkasten"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))
