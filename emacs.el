
;;
;; bootstrap use-package for all
;; later packages
;;

(require 'package)
(setq package-enable-at-startup nil)
;; add additional sources to package list
(dolist (repo
         '(("melpa" . "http://melpa.org/packages/")
	   ("org" . "http://orgmode.org/elpa/")
           ("marmalade" . "https://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives repo))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;
;; global key mappings
;;

;; rethink this key mapping; help functions are super useful
;; (global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-M-i") 'indent-rigidly)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;
;; global appearance settings
;;

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(add-to-list 'default-frame-alist
             '(font . "Fantasque Sans Mono-12"))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (and (not (eq system-type 'darwin)) ;; keep in OSX b/c it doesn't take up extra space
           (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(global-visual-line-mode 1)
(setq ring-bell-function 'ignore)

;;
;; functional customizations
;;

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 20000000)

(setq make-backup-files nil)

;; if mac/linux, load path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-auto-revert-mode t)

;; tramp use plink on windows, SSH otherwise
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; save session when quitting
(when (fboundp 'desktop-save-mode)
    (require 'desktop)
    (desktop-save-mode 1)
    
    (defun my-desktop-save ()
      (interactive)
      ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
      (if (eq (desktop-owner) (emacs-pid))
          (desktop-save desktop-dirname)))
    (add-hook 'auto-save-hook 'my-desktop-save)

    (setq desktop-path '("~/.emacs.d/desktop/"))
    (setq desktop-dirname "~/.emacs.d/desktop/")
    (setq desktop-base-file-name ".emacs.desktop")

    ;; remove desktop after it's been read
    (add-hook 'desktop-after-read-hook
              '(lambda ()
                 ;; desktop-remove clears desktop-dirname
                 (setq desktop-dirname-tmp desktop-dirname)
                 (desktop-remove)
                 (setq desktop-dirname desktop-dirname-tmp)))
    
    (defun saved-session ()
      (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))    

    ;; use session-restore to restore the desktop manually
    (defun session-restore ()
      "Restore a saved emacs session."
      (interactive)
      (if (saved-session)
          (desktop-read)
        (message "No desktop found.")))
    
    ;; use session-save to save the desktop manually
    (defun session-save ()
      "Save an emacs session."
      (interactive)
      (if (saved-session)
          (if (y-or-n-p "Overwrite existing desktop? ")
              (desktop-save-in-desktop-dir)
            (message "Session not saved."))
        (desktop-save-in-desktop-dir)))
    
    ;; ask user whether to restore desktop at start-up
    (add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))
    )

;;
;; custom functions
;;

(defun swap-buffers ()
  "Move the current buffer into the next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
		 (other (next-window))
		 (this-buffer (window-buffer this))
		 (other-buffer (window-buffer other)))
	(set-window-buffer other this-buffer)
	(set-window-buffer this other-buffer)))

(defun middle-of-line ()
  "Put cursor at middle point of the line."
  (interactive)
  (goto-char (/ (+ (point-at-bol) (point-at-eol)) 2)))
(global-set-key "\M-g\M-l" 'middle-of-line)

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))


;;
;; eshell
;;

(defun eshell-clear-buffer()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(eshell-send-input)))
(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; to run a command in the background, run ec [command]
 (defun eshell/ec (&rest args)
      "Use `compile' to do background makes."
      (if (eshell-interactive-output-p)
          (let ((compilation-process-setup-function
                 (list 'lambda nil
                       (list 'setq 'process-environment
                             (list 'quote (eshell-copy-environment))))))
            (compile (eshell-flatten-and-stringify args))
            (pop-to-buffer compilation-last-buffer))
        (throw 'eshell-replace-command
               (let ((l (eshell-stringify-list (eshell-flatten-list args))))
                 (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

;; aliases
(defun eshell/emacs (file)
  (find-file file))
(defun eshell/open (file)
  (find-file-other-window file))
(defun eshell/xb ()
  (ido-switch-buffer))
(defun eshell/xo ()
  (other-window 1))


;;
;; built-in mode-specific settings
;;

;; python mode
(add-hook 'python-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; SQL mode
(add-to-list 'auto-mode-alist '("\\.prc\\'" . sql-mode))
;; (add-hook 'sql-mode-hook '(lambda()
;;                             (setq tab-width 4)
;;                             (setq indent-tabs-mode nil)))
(setq sql-sqlite-program "sqlite3")
(setq sql-connection-alist
      '((fixd-redshift (sql-product 'postgres)
                       (sql-server "redshift-cluster-1.c9hwoc0lav0f.us-east-2.redshift.amazonaws.com")
                       (sql-port 5439)
                       (sql-database "analytics")
                       (sql-user "apope"))))
(setq sql-send-terminator t)


;;
;; package settings
;;

;; ido mode
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (setq ido-file-extension-order '(".py", ".txt", ".org"))
  (ido-mode t))

(use-package ido-completing-read+
  :ensure t)

;; ace window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-background t)
  (ace-window-display-mode 1)
  (set-face-attribute `aw-leading-char-face nil
                      :height 1.0 :foreground "#fb4933" :background "#282828" :bold t)
  ;; tried to use the below to get the overlay to dim text properly, but it doesn't work
  ;; my best guess is that the theme I'm using is overwriting it somehow
  (set-face-attribute `aw-background-face nil
                    :inverse-video nil :background "#282828" :foreground "#7e7a60"))

;; jedi mode
(use-package jedi
  :ensure t
  :hook ((python-mode . jedi:setup)))

;; ein mode
(use-package ein
  :ensure t
  ;; only loads if it's a GUI interfaces
  :if window-system
  ;; :requires jedi
  :hook ((ein:connect-mode . ein:jedi-setup))
  ;; set these immediately so the first connection works
  :init
  (setq ein:jupyter-default-server-command "jupyter")
  (setq ein:jupyter-default-notebook-directory "/home/apope/")
  (setq ein:jupyter-server-args (list "--no-browser"))
  (setq ein:default-url-or-port "https://home.andrewapope.com:8888/")
  :config
  (setq ein:notebook-modes '(ein:notebook-multilang-mode))
  (setq ein:enable-keepalive t)
  (setq ein:notebooklist-enable-keepalive t) ;; not sure which variable is correct!
  ;; better image scrolling, but some bugs
  (setq ein:slice-image t)
  (setq ein:completion-backend 'ein:use-ac-jedi-backend)
  ;; (setq ein:worksheet-enable-undo t) <- didn't do what i wanted
  ;; (setq ein:polymode t) <- caused all sorts of weird shit
  (setq ein:worksheet-enable-undo t)
  ;; (ein:org-register-lang-mode "ein-python" `python)
  )

;; yaml mode
(use-package yaml-mode
  :ensure t
  :config
  (setq yaml-indent-offset 2))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "/usr/bin/pandoc")
  (setq markdown-header-scaling t))

;; mmm
;; add jinja mode to SQL files for dbt usage
(use-package mmm-mode
  :ensure t
  :config
  ;; can add this back later: functionality to remove trailing whitespace
  ;; (add-hook 'mmm-mode-hook
  ;;           (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
  (setq mmm-global-mode 'maybe))


(use-package mmm-jinja2
  :ensure t
  :requires mmm-mode
  :config
  (mmm-add-mode-ext-class 'sql-mode nil 'jinja2))

;; magit
(use-package magit
  :ensure t
  :after (ido ido-completing-read+)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-completing-read-function 'magit-ido-completing-read))

;; with-editor mode
(use-package with-editor
  :ensure t
  :if window-system
  :hook ((eshell-mode . with-editor-export-editor))
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command))
  :config
  ;; (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  ;; (define-key (current-global-map)
  ;;   [remap async-shell-command] 'with-editor-async-shell-command)
  ;; (define-key (current-global-map)
  ;;   [remap shell-command] 'with-editor-shell-command)
  ;; with-editor for crontab
  (defun crontab-e ()
    (interactive)
    ;; requires with-editor package
    (with-editor-async-shell-command "crontab -e")))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package sql-indent
  :ensure t
  :init
  (defvar aap-sql-indentation-offsets-alist
    '((nested-statement-open sqlind-use-anchor-indentation
                             +)
      (nested-statement-continuation 1)
      (nested-statement-close sqlind-use-anchor-indentation)
      (select-clause 0)
      (select-column sqlind-indent-select-column)
      (select-column-continuation ++)
      (select-table 0)
      (select-table-continuation sqlind-lineup-joins-to-anchor)
      (select-join-condition +)
      (in-select-clause +
                        sqlind-lone-semicolon)
      (case-clause +)
      (case-clause-item-cont +)))
  (defun aap-set-sql-indent ()
    (setq sqlind-indentation-offsets-alist aap-sql-indentation-offsets-alist)
    (setq sqlind-basic-offset 4))
  :hook ((sql-mode . sqlind-minor-mode)
         (sqlind-minor-mode . aap-set-sql-indent)))


;;
;; org
;;



(use-package org
  :ensure t
  :demand t ;; so the packages which depend on org load properly
  :init ;; let's define the functions here
  ;; org-mode agenda files
  ;; Set protocol based on OS type
  ;; Only setup function, make emacs ask.
  (defun org-load-files ()
    (interactive)
    (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
        (progn
          (setq org-agenda-files '("/plink:apope@andrewapope.com:/home/apope/orgs/agenda"))
          (setq org-personal-notes-file "/plink:apope@andrewapope.com:/home/apope/orgs/notes/personal_notes.org")
          (setq org-work-notes-file "/plink:apope@andrewapope.com:/home/apope/orgs/notes/work_notes.org")
          (setq org-personal-agenda-file "/plink:apope@andrewapope.com:/home/apope/orgs/agenda/personal.org")
          (setq org-work-agenda-file "/plink:apope@andrewapope.com:/home/apope/orgs/agenda/work.org"))
      (if (eq system-type 'darwin)
          (progn
            ;; possibly BROKEN
            (setq org-agenda-files '("/ssh:aap:/home/apope/orgs/agenda"))
            (setq org-personal-notes-file "/ssh:aap:/home/apope/orgs/notes/personal_notes.org")
            (setq org-work-notes-file "/ssh:aap:/home/apope/orgs/notes/work_notes.org")
            (setq org-personal-agenda-file "/ssh:aap:/home/apope/orgs/agenda/personal.org")
            (setq org-work-agenda-file "/ssh:aap:/home/apope/orgs/agenda/work.org"))
        ;; these are correct and have been edited for the sabbatical + contract work
        (setq org-agenda-files '("/ssh:aap:/home/apope/orgs/agenda"))
        (setq org-personal-notes-file "/ssh:aap:/home/apope/orgs/notes/personal_notes.org")
        (setq org-work-notes-file "/ssh:aap:/home/apope/orgs/notes/work_notes.org")
        (setq org-personal-agenda-file "/ssh:aap:/home/apope/orgs/agenda/personal.org")
        (setq org-work-agenda-file "/ssh:aap:/home/apope/orgs/agenda/work.org"))))
  ;; reload functionality to enable mobile tasks
  ;; prepends the org agenda reload with a file revert
  (defun aap-org-agenda-reload ()
    (interactive)
    (with-current-buffer "personal.org"
      (revert-buffer t t t))
    (with-current-buffer "work.org"
      (revert-buffer t t t))
    (with-current-buffer "triage.org"
      (revert-buffer t t t))
    (org-agenda-redo t))
  (defun aap-indent-project ()
    (if (string-equal (org-entry-get (point) "TODO") "PROJECT")
        (make-string (* (org-current-level) 2) ?\s)
      (concat
       (make-string (* (- (org-current-level) 1) 2) ?\s)
       "- ")))
    ;;; Support for links to Outlook items in Org
    ;;; To find the corresponding Outlook macro (and for the source of this code), go to:
    ;;; https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
    ;;; Make sure to enable Microsoft Forms 2.0 reference for that macro!
  (defun org-outlook-open (id)
    "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
    (w32-shell-execute "open" "C:/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE" (concat "/select " "outlook:" id)))
  
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
         ;; :map org-agenda-mode-map
         ;; ("r" . aap-org-agenda-reload))
  
  :config
  (add-hook 'org-agenda-mode-hook '(lambda ()
                                     (local-set-key (kbd "r") 'aap-org-agenda-reload)))
  (setq org-log-into-drawer t)
  (setq org-reverse-note-order nil)
  (setq org-log-state-notes-insert-after-drawers nil)
  (setq org-insert-heading-respect-content nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-completion-use-ido t)
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-hide-emphasis-markers t)
  (setq org-use-property-inheritance t)

  ;; math stuff
  ;; reminder: C-c C-x C-l turns on/off latex preview
  ;; C-c C-x \ turns on/off pretty entities
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t)
  ;; ignoring $1, $ and $$ as matchers
  (setq org-format-latex-options
        '(:foreground default :background default :scale 1.5
          :html-foreground "Black" :html-background "Transparent"
          :html-scale 1.5 :matchers ("begin" "\\(" "\\[")))
  (setq org-startup-indented t)

  (setq org-babel-load-languages '((emacs-lisp . t) (ein . t)))
  
  (org-add-link-type "outlook" 'org-outlook-open)
  ;; todo types, sequences, and templates
  (setq org-todo-keywords
	'((sequence "TODO(t!)" "|" "DONE(d!/!)")
	  (sequence "BLOCKED(b@/!)")
	  (sequence "|" "CANCELED(c@)")
          (sequence "ONHOLD(o!/!)")
	  (sequence "URGENT(u)")
          (sequence "IN-QA(q!/@)")
          (sequence "DO-QA(a@)" "|" "DONE(d!/!)")
          (sequence "IDEA(i!)" "|")
          (sequence "NEXT(n!)" "|")
          (sequence "PROJECT(p!)" "|" "DONE(d!/!)")
          (sequence "PENDING(g!)")))
  (setq org-todo-keyword-faces
	'(("TODO" . "orange") ("BLOCKED" . "yellow") ("CANCELED" . "green")
	  ("DONE" . "green") ("URGENT" . "red") ("IN-QA" . "yellow")
          ("DO-QA" . "orange") ("PROJECT" . (:foreground "dark orchid" :weight bold))
          ("IDEA" . (:foreground "royal blue" :weight bold))
          ("NEXT". "IndianRed1")
          ("ONHOLD" . "yellow")
          ("PENDING" . (:foreground "deep sky blue" :weight bold))))
  (setq org-tag-alist
        '((:startgroup nil)
          ("reading" . ?r) ("lecture" . ?l) ("problem" . ?p) ("writing" . ?w)
          ("exercise" . ?e) ("admin" . ?a)
          (:endgroup nil)
          (:startgroup nil)
          ("data" . ?d) ("science" . ?s) ("philosophy" . ?y) ("social" . ?c)
          (:endgroup nil)))
  (setq org-capture-templates
        '(;; work templates
          ("w" "Work templates")
          ("wd" "Work Todo with deadline" entry (file org-work-agenda-file)
           "* TODO %?\n  DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+fri\"))\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("wt" "Work Todo without deadline" entry (file org-work-agenda-file)
           "* TODO %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("wn" "Work Note" entry (file org-work-notes-file)
           "* %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("wi" "Work Idea" entry (file org-work-agenda-file)
           "* IDEA %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("wp" "Work Project" entry (file org-work-agenda-file)
           "* PROJECT %^{Project Name}%? [/] :project:\n  :PROPERTIES:\n  :CATEGORY: %\\1 \n  :END:\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ;; personal templates
          ("p" "Personal templates")
          ("pd" "Personal Todo with deadline" entry (file org-personal-agenda-file)
           "* TODO %?\n  DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+fri\"))\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("pt" "Personal Todo without deadline" entry (file org-personal-agenda-file)
           "* TODO %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("pn" "Personal Note" entry (file org-personal-notes-file)
           "* %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("pi" "Personal Idea" entry (file org-personal-agenda-file)
           "* IDEA %?\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ("pp" "Personal Project" entry (file org-personal-agenda-file)
           "* PROJECT %^{Project Name}%? [/] :project:\n  :PROPERTIES:\n  :CATEGORY: %\\1 \n  :END:\n  :LOGBOOK:\n  - Created\t%U\n  :END:")
          ;; unique templates
          ("j" "Journal entry" entry (file org-personal-notes-file)
           "* Journal Entry - %T\n  %?")))
  (setq org-stuck-projects
        '("/+PROJECT" ("TODO" "BLOCKED" "URGENT" "DO-QA" "NEXT") nil))
          ;; "SCHEDULED:\\|DEADLINE:"))
  (setq org-hierarchical-todo-statistics t)
  (setq org-completion-use-ido t)
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps t)
  (setq org-agenda-custom-commands
        '(
          ("n" "Traditional combined view"
           ((agenda "")
            (alltodo "")))
          ("p" "Personal tasks"
           ((todo "PENDING"
                  ((org-agenda-overriding-header "New tasks pending triage:")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Tasks to do next:")))
            (agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-sorting-strategy
                      (quote
                       (priority-down todo-state-up)))
                     (org-deadline-warning-days 6)
                     (org-agenda-deadline-faces
                      '((1.0 . org-warning)
                        (0.33 . org-upcoming-deadline)
                        (0.0 . default)))
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "Agenday for today:")))
            (todo "TODO|DO-QA"
                       ((org-agenda-sorting-strategy
                         (quote
                          (todo-state-down priority-down alpha-up)))
                        (Org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-overriding-header "Unscheduled todo items:")
                        (org-agenda-todo-ignore-with-date t)))
            (stuck ""
                   ((org-agenda-prefix-format " ")
                    (org-agenda-todo-ignore-with-date t)
                    (org-agenda-overriding-header "Projects with no tasks:")))
            (todo "IDEA"
                  ((org-agenda-overriding-header "Ideas:")
                   (org-agenda-sorting-strategy
                    (quote
                     (priority-down alpha-up)))))
            (todo "IN-QA|BLOCKED"
                  ((org-agenda-overriding-header "Items blocked or in QA:")
                   (org-agenda-sorting-strategy
                    (quote
                     (priority-down alpha-up))))))
           ((org-agenda-tag-filter-preset '("+personal"))
            (org-agenda-prefix-format " %i %-22:c ")
            (org-agenda-dim-blocked-tasks nil)))

          ("w" "Work tasks"
           ((todo "PENDING"
                  ((org-agenda-overriding-header "New tasks pending triage:")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Tasks to do next:")))
            (agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-sorting-strategy
                      (quote
                       (priority-down todo-state-up)))
                     (org-deadline-warning-days 6)
                     (org-agenda-deadline-faces
                      '((1.0 . org-warning)
                        (0.33 . org-upcoming-deadline)
                        (0.0 . default)))
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "Agenday for today:")))
            (todo "TODO|DO-QA"
                       ((org-agenda-sorting-strategy
                         (quote
                          (todo-state-down priority-down alpha-up)))
                        (Org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-overriding-header "Unscheduled todo items:")
                        (org-agenda-todo-ignore-with-date t)))
            (stuck ""
                   ((org-agenda-prefix-format " ")
                    (org-agenda-todo-ignore-with-date t)
                    (org-agenda-overriding-header "Projects with no tasks:")))
            (todo "IDEA"
                  ((org-agenda-overriding-header "Ideas:")
                   (org-agenda-sorting-strategy
                    (quote
                     (priority-down alpha-up)))))
            (todo "IN-QA|BLOCKED"
                  ((org-agenda-overriding-header "Items blocked or in QA:")
                   (org-agenda-sorting-strategy
                    (quote
                     (priority-down alpha-up))))))
           ((org-agenda-tag-filter-preset '("+work"))
            (org-agenda-prefix-format " %i %-22:c ")
            (org-agenda-dim-blocked-tasks nil)))
          
          ("o" "On hold projects"
           todo "ONHOLD"
           ((org-agenda-prefix-format "  ")))
          ("d" "Completed todos scheduled in the last 14 days"
           todo "DONE"))))

(use-package org-bullets
  :ensure t
  :requires org
  :demand t
  ;; not 100% sure why, but this wasn't working
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq org-bullets-bullet-list '("✸" "◉" "✿" "○")))



;;
;; random shit I'm playing around with
;;

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifications))
