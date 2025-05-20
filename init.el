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
         ("M-y" . consult-yank-pop)
	 ("C-x C-r" . consult-recent-file)))
(recentf-mode 1)
;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
;; org-mode
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/zettelkasten"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))
(use-package websocket
  :after org-roam)
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
;; org-capture
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/stocks")
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("r" "Reading Log" entry
         (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/stocks/reading-log.org" "2025")
         "** %u 『%^{書名}』 %^{著者}
:PROPERTIES:
:FINISHED: %u
:CONTEXT: %^{文脈|TOC理論|リーダーシップ|COO|経営|組織論|個人戦略|}
:END:
*** 所感 %?" :empty-lines 1)
      ("j" "Journal" entry
         (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/weekly_reviews/journal.org" "2025")
         "* %<%Y-%m-%d>\n*** 今日なにした？\n- %?\n*** どう感じた？\n- \n*** 明日はどうしたい？\n- \n"
         :prepend t)))
(use-package org-ql
  :after org
  :ensure t)
(defun my/new-draft ()
  "Create a new draft org file with a timestamped filename and title, including Japanese."
  (interactive)
  (let* ((draft-dir "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/zettelkasten/drafts/")
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
  (let ((file-name "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/weekly_reviews/2025-weekly-reviews.org"))
    (find-file file-name)
    (when (= (buffer-size) 0)
      (insert "#+TITLE: Weekly Reviews\n")
      (insert "#+AUTHOR: あなたの名前\n")
      (insert "#+OPTIONS: toc:2\n\n"))))
(defun add-new-weekly-review ()
  "現在の週の新しいWeekly Reviewエントリを追加する（月曜始まり日曜終わり）"
  (interactive)
  (open-weekly-reviews-file)
  (goto-char (point-max))
  (let* ((time (current-time))
         (year (format-time-string "%Y" time))
         (week (format-time-string "%V" time))
         (current-day-of-week (string-to-number (format-time-string "%u" time))) ; 1=月曜, 7=日曜
         (days-since-monday (- current-day-of-week 1)) ; 月曜日からの経過日数
         (days-until-sunday (- 7 current-day-of-week)) ; 日曜日までの残り日数
         (week-start (format-time-string "%Y-%m-%d" 
                                        (time-subtract time 
                                                      (days-to-time days-since-monday))))
         (week-end (format-time-string "%Y-%m-%d"
                                      (time-add time
                                               (days-to-time days-until-sunday)))))
    (insert (format "\n* %s-W%s (%s 〜 %s)\n" year week week-start week-end))
    (insert "** 1. What Happened （今週の出来事・やったこと、事実ベース）\n- \n\n")
    (insert "** 2. What Went Well （よかったこと・うまくいったこと）\n- \n\n")
    (insert "** 3. What Didn't Go Well （しんどかったこと・詰まったこと）\n- \n\n")
    (insert "** 4. What to Adjust （来週改善したいこと・やめる／始める）\n- \n\n")
    (insert "** 5. Still Thinking... （もやもや・未整理・考え中のこと）\n- \n")))
