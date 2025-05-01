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
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil)
(setq truncate-lines nil)
;; share clipboard with OS
(setq select-enable-clipboard t)

;; fundamental packages
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)))


(use-package org-roam
  :custom
  (org-roam-directory (file-truename "/Users/hikaru/Library/Mobile Documents/com~apple~CloudDocs/Documents/zettelkasten"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))


(defun my/new-draft ()
  "Create a new draft org file with a timestamped filename and title, including Japanese."
  (interactive)
  (let* ((draft-dir "/Users/hikaru/Library/Mobile Documents/com~apple~CloudDocs/Documents/zettelkasten/drafts/")
         (title (read-string "Draft title: "))
         ;; スペースだけアンダーバー、それ以外はそのまま
         (sanitized-title (replace-regexp-in-string " " "_" title))
         (filename (format "%s_%s.org"
                           (format-time-string "%Y%m%d")
                           sanitized-title))
         (filepath (expand-file-name filename draft-dir)))
    (find-file filepath)
    (insert (format "#+title: %s\n\n" title))))

(defun open-weekly-reviews-file ()
  "Weekly Reviewsファイルを開く"
  (interactive)
  (let ((file-name "/Users/hikaru/Library/Mobile Documents/com~apple~CloudDocs/Documents/weekly_reviews/2025-weekly-reviews.org"))
    (find-file file-name)
    (when (= (buffer-size) 0)
      (insert "#+TITLE: Weekly Reviews\n")
      (insert "#+AUTHOR: あなたの名前\n")
      (insert "#+OPTIONS: toc:2\n\n"))))

(defun add-new-weekly-review ()
  "現在の週の新しいWeekly Reviewエントリを追加する"
  (interactive)
  (open-weekly-reviews-file)
  (goto-char (point-max))
  (let* ((time (current-time))
         (year (format-time-string "%Y" time))
         (week (format-time-string "%V" time))
         (week-start (format-time-string "%Y-%m-%d" 
                                        (time-subtract time 
                                                      (days-to-time (string-to-number (format-time-string "%u" time))))))
         (week-end (format-time-string "%Y-%m-%d"
                                      (time-add time
                                               (days-to-time (- 7 (string-to-number (format-time-string "%u" time))))))))
    (insert (format "\n* %s-W%s (%s 〜 %s)\n" year week week-start week-end))
    (insert "📌** 1. What Happened\n**（今週の出来事・やったこと、事実ベース）**\n- \n\n")
    (insert "🌟** 2. What Went Well\n**（よかったこと・うまくいったこと）**\n- \n\n")
    (insert "💥** 3. What Didn't Go Well\n**（しんどかったこと・詰まったこと）**\n- \n\n")
    (insert "🔁** 4. What to Adjust\n**（来週改善したいこと・やめる／始める）**\n- \n\n")
    (insert "🌱** 5. Still Thinking...\n**（もやもや・未整理・考え中のこと）**\n- \n")))
