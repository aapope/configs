
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
(add-hook 'sql-mode-hook '(lambda()
                            (setq tab-width 4)
                            (setq indent-tabs-mode nil)))


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

;; ace window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

;; jedi mode
(use-package jedi
  :ensure t
  :hook ((python-mode . jedi:setup)))

;; ein mode
(use-package ein
  :ensure t
  :after jedi
  :hook ((ein:connect-mode . ein:jedi-setup))
  :config
  (setq ein:notebook-modes '(ein:notebook-multilang-mode))
  (setq ein:jupyter-default-server-command "jupyter")
  ;; (setq ein:jupyter-default-notebook-directory "b:")
  (setq ein:jupyter-server-args (list "--no-browser"))
  (setq ein:enable-keepalive t)
  (setq ein:notebooklist-enable-keepalive t) ;; not sure which variable is correct!
  ;; better image scrolling, but some bugs
  (setq ein:slice-image t))

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
  (setq markdown-command "/usr/local/bin/pandoc"))

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
  :after ido
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


;;
;; org
;;



(use-package org-bullets
  :ensure t
  :requires org
  :hook ((org-mode . org-bullets-mode)))

(use-package org
  :ensure t
  
  :init ;; let's define the functions here
  ;; org-mode agenda files
  ;; Set protocol based on OS type
  ;; Only setup function, make emacs ask.
  (defun org-load-files ()
    (interactive)
    (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
        (progn
          (setq org-agenda-files '("/plink:apope@andrewapope.com:~/orgs"))
          (setq org-default-notes-file "/plink:apope@andrewapope.com:/home/apope/orgs/work.org")
          (setq aap-notes-file "C:/Users/apope/Documents/work_notes.org")
          (setq aap-personal-file "/plink:apope@andrewapope.com:/home/apope/orgs/personal.org"))
      (if (eq system-type 'darwin)
          (progn
            (setq org-agenda-files '("/ssh:apope@andrewapope.com:~/orgs"))
            (setq org-default-notes-file "/ssh:apope@andrewapope.com:/home/apope/orgs/work.org")
            (setq aap-notes-file "/Users/apope/notes.org")
            (setq aap-personal-file "/ssh:apope@andrewapope.com:/home/apope/orgs/personal.org"))
        (setq org-agenda-files '("/ssh:apope@andrewapope.com:~/orgs"))
        (setq org-default-notes-file "/ssh:apope@andrewapope.com:/home/apope/orgs/work.org")
        (setq aap-notes-file "/ssh:apope@andrewapope.com:~/orgs/personal_notes.org")
        (setq aap-personal-file "/ssh:apope@andrewapope.com:/home/apope/orgs/personal.org"))))
  ;; reload functionality to enable mobile tasks
  ;; prepends the org agenda reload with a file revert
  (defun aap-org-agenda-reload ()
    (interactive)
    (with-current-buffer "work.org"
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
  (org-add-link-type "outlook" 'org-outlook-open)
  ;; todo types, sequences, and templates
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!/!)")
	  (sequence "BLOCKED(b@/!)")
	  (sequence "|" "CANCELED(c@)")
	  (sequence "URGENT(u)")
          (sequence "IN-QA(q!/@)")
          (sequence "DO-QA(a@)" "|" "DONE(d!/!)")
          (sequence "IDEA(i)" "|")
          (sequence "NEXT(n!)" "|")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)")
          (sequence "PENDING(g)")))
  (setq org-todo-keyword-faces
	'(("TODO" . "orange") ("BLOCKED" . "yellow") ("CANCELED" . "green")
	  ("DONE" . "green") ("URGENT" . "red") ("IN-QA" . "yellow")
          ("DO-QA" . "orange") ("PROJECT" . (:foreground "dark orchid" :weight bold))
          ("IDEA" . (:foreground "royal blue" :weight bold))
          ("NEXT". "IndianRed1")
          ("PENDING" . (:foreground "deep sky blue" :weight bold))))
  (setq org-tag-alist
        '((:startgroup nil)
          ("work" . ?w) ("personal" . ?p)
          (:endgroup nil)
          ("project". ?j)))
  (setq org-capture-templates
        '(("d" "Todo with deadline" entry (file "")
           "* TODO %?\n DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+fri\"))")
          ("t" "Todo without deadline" entry (file "")
           "* TODO %?")
          ("q" "Do-QA" entry (file "")
           "* DO-QA %?\n DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+fri\"))\n :PROPERTIES:\n :CATEGORY: DO-QA\n :END:\n %x")
          ("n" "Note" entry (file aap-notes-file)
           "* %?")
          ("m" "Meeting note" entry (file aap-notes-file)
           "* %^{Meeting topic} - %T\n  - %?")
          ("i" "Idea" entry (file "")
           "* IDEA %?")
          ("p" "Project" entry (file "")
           "* PROJECT %^{Project Name}%? [%] :project:\n  :PROPERTIES:\n  :CATEGORY: %\\1 \n  :END:")
          ("j" "Personal project" entry (file aap-personal-file)
           "* PROJECT %^{Project Name}%? [%] :project:\n  :PROPERTIES:\n  :CATEGORY: %\\1 \n  :END:")
          ("o" "Personal todo item" entry (file aap-personal-file)
           "* TODO %?")))
  (setq org-agenda-custom-commands
        '(
          ("n" "Traditional combined view"
           ((agenda "")
            (alltodo "")))
          ("w" "Work tasks"
           ((todo "PENDING"
                  ((org-agenda-overriding-header "New tasks pending triage:")
                   (org-agenda-prefix-format " %i %-15:c ")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Items to do next:")
                   (org-agenda-prefix-format " %i %-15:c ")))
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
                     (org-agenda-prefix-format " %i %-15:c%?-12t% s")
                     (org-agenda-overriding-header "Agenday for today:")))
            (tags-todo "-project/TODO|DO-QA|BLOCKED|IN-QA"
                       ((org-agenda-sorting-strategy
                         (quote
                          (todo-state-down priority-down alpha-up)))
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-overriding-header "Other todo items:")
                        (org-agenda-todo-ignore-with-date t)))
            (tags-todo "+project/-DONE-CANCELLED"
                       ((org-agenda-overriding-header "Ongoing projects:")
                        (org-agenda-prefix-format " %i %-15:c   %(aap-indent-project)")
                        (org-agenda-sorting-strategy
                         (quote
                          (category-keep)))))
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
            (org-agenda-dim-blocked-tasks nil)))

          ("p" "Personal tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Items to do next:")
                   (org-agenda-prefix-format " %i %-15:c ")))
            (tags-todo "+project/-DONE-CANCELLED"
                       ((org-agenda-overriding-header "Ongoing projects:")
                        (org-agenda-prefix-format " %i %-15:c   %(aap-indent-project)")
                        (org-agenda-sorting-strategy
                         (quote
                          (category-keep)))))
            (todo "IDEA"
                  ((org-agenda-overriding-header "Ideas:")
                   (org-agenda-sorting-strategy
                    (quote
                     (priority-down alpha-up)))))
            (tags-todo "-project/TODO|DO-QA|BLOCKED|IN-QA"
                       ((org-agenda-sorting-strategy
                         (quote
                          (todo-state-down priority-down alpha-up)))
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-overriding-header "Other todo items:")
                        (org-agenda-todo-ignore-with-date t))))
           ((org-agenda-tag-filter-preset '("+personal"))))
          ("d" "Completed todos scheduled in the last 14 days"
           todo "DONE"))))


;;
;; random shit I'm playing around with
;;

;; slack
;; (el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "SALTED"
   :default t
   :client-id "fb6bec2f-1551368819.663"
;;   :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   :token "xoxs-412606013072-413326318658-454014502502-46fe595ec008a99bb7b369a0a75bea2a540cfe574b0b81c895a82c30299b0d67"
   :subscribed-channels '(general random)
   :full-and-display-names t)

  ;; (evil-define-key 'normal slack-info-mode-map
  ;;   ",u" 'slack-room-update-messages)
  ;; (evil-define-key 'normal slack-mode-map
  ;;   ",c" 'slack-buffer-kill
  ;;   ",ra" 'slack-message-add-reaction
  ;;   ",rr" 'slack-message-remove-reaction
  ;;   ",rs" 'slack-message-show-reaction-users
  ;;   ",pl" 'slack-room-pins-list
  ;;   ",pa" 'slack-message-pins-add
  ;;   ",pr" 'slack-message-pins-remove
  ;;   ",mm" 'slack-message-write-another-buffer
  ;;   ",me" 'slack-message-edit
  ;;   ",md" 'slack-message-delete
  ;;   ",u" 'slack-room-update-messages
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel
  ;;   "\C-n" 'slack-buffer-goto-next-message
  ;;   "\C-p" 'slack-buffer-goto-prev-message)
  ;;  (evil-define-key 'normal slack-edit-message-mode-map
  ;;   ",k" 'slack-message-cancel-edit
  ;;   ",s" 'slack-message-send-from-buffer
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel))
)

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
