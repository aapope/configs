; set custom key codes
(put 'upcase-region 'disabled nil) 
(global-set-key "\C-h" 'backward-delete-char) 
(global-set-key "\C-w" 'backward-kill-word) 
(global-set-key "\C-x\C-k" 'kill-region) 
(global-set-key "\C-c\C-k" 'kill-region) 
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
 
;; python settings 
(add-hook 'python-mode-hook '(lambda () 
  (local-set-key (kbd "RET") 'newline-and-indent))) 
 
;; update SQL mode to open .prc 
(add-to-list 'auto-mode-alist '("\\.prc\\'" . sql-mode))
(add-hook 'sql-mode-hook '(lambda()
                            (setq tab-width 4)
                            (setq indent-tabs-mode nil)))
;; NOTE: when i tested this, there were mismatched parens. Try it again later!!
;; (add-hook 'sqlplus-mode-hook '(lambda()
;;                                 (setq tab-width 4)
;;                                 (setq indent-tabs-mode nil)
;;                                 (define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)))

;; (setq tab-stop-list (number-sequence 2 120 2))
 
;; add default markdown mode extensions 
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) 
 
;; remove backup files because I hate them 
(setq make-backup-files nil) 
 
;; wrap lines 
(global-visual-line-mode 1)
 
;; code folding 
(if (>= emacs-major-version 24) 
    (add-hook 'prog-mode-hook #'hs-minor-mode) 
  (add-hook 'python-mode-hook #'hs-minor-mode))

;; tramp stuff
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

 
;; for non-terminal-based emacs: 
;; remove tool bars, set theme and font, 
;; and set background transparency 
(defun load-gui-settings () 
  (tool-bar-mode -1) 
  (scroll-bar-mode -1) 
  (load-theme 'tango-dark) 
  ;; (set-default-font "Fantasque Sans Mono-12")
  (add-to-list 'default-frame-alist
               '(font . "Fantasque Sans Mono-14"))
  ;; semi-transparent background 
  ;; (set-frame-parameter (selected-frame) 'alpha '(85 85)) 
  ;; (add-to-list 'default-frame-alist '(alpha 85 85)) 
  ) 
 
(if (display-graphic-p) 
    (load-gui-settings)) 
 
 
;; if we have a package manager, install default 
;; packages and customize 
(defun setup-all-packages () 
  (when  (require 'package nil 'noerror) 
    ;; set package manager archives 
    (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			     ("marmalade" . "https://marmalade-repo.org/packages/") 
			     ("melpa" . "http://melpa.org/packages/") 
			     ("org" . "http://orgmode.org/elpa/"))) 
    (package-initialize) 
     
     
    ;; checks list of required packages, installs if not available 
    (require 'cl) 
    (defvar prelude-packages 
      '(org ein with-editor ess ido magit) 
      "A list of packages that need to be installed at launch.") 
     
    (defun prelude-packages-installed-p () 
      (loop for p in prelude-packages 
	    when (not (package-installed-p p)) do (return nil) 
	    finally (return t))) 
     
    (if (>= emacs-minor-version 4) 
	(unless (prelude-packages-installed-p) 
	  ;; check for new packages (packages versions) 
	  (message "%s" "Emacs is now refreshing package database...") 
	  (package-refresh-contents) 
	  (message "%s" "done") 
	  ;; install missing packages 
	  (dolist (p prelude-packages) 
	    (when (not (package-installed-p p)) 
	      (package-install p))))) 
     
    (provide 'prelude-packages) 
    ;; end package installs  
    
    ;; custom keys for org-mode 
    (global-set-key "\C-cl" 'org-store-link) 
    (global-set-key "\C-ca" 'org-agenda) 
    (global-set-key "\C-cb" 'org-iswitchb) 
    (global-set-key "\C-cc" 'org-capture) 
     
    ;; custom settings for org-mode 
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
    
    (add-hook 'org-agenda-mode-hook '(lambda ()
                                       (local-set-key (kbd "r") 'aap-org-agenda-reload)))

    
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

    (defun aap-indent-project ()
       (if (string-equal (org-entry-get (point) "TODO") "PROJECT")
           (make-string (* (org-current-level) 2) ?\s)
         (concat
           (make-string (* (- (org-current-level) 1) 2) ?\s)
           "- ")))
 
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
             todo "DONE")))


            
     
    ;; enable python for in-buffer evaluation 
    (org-babel-do-load-languages 
     'org-babel-load-languages 
     '((python . t) 
       (java . t))) 
 
    ;;; Support for links to Outlook items in Org 
    ;;; To find the corresponding Outlook macro, go to (and for the source of this code): 
    ;;; https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook 
    ;;; Make sure to enable Microsoft Forms 2.0 reference for that macro! 
    (defun org-outlook-open (id) 
      "Open the Outlook item identified by ID.  ID should be an Outlook GUID." 
      (w32-shell-execute "open" "C:/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE" (concat "/select " "outlook:" id))) 
    (org-add-link-type "outlook" 'org-outlook-open) 
     
    ;; use orgstruct++ minor mode in python code 
    ;; useful for extended documentation sections 
    ;; if emacs major mode is >= 24, turn on for all 
    ;; programming modes 
    (if (>= emacs-major-version 24) 
	(add-hook 'prog-mode-hook #'orgstruct++-mode) 
      (add-hook 'python-mode-hook #'orgstruct++-mode)) 
     
    ;; jedi for python mode 
    (add-hook 'python-mode-hook 'jedi:setup) 
     
    ;; ido mode 
    (setq ido-enable-flex-matching t) 
    (setq ido-everywhere t) 
    (setq ido-file-extension-order '(".py", ".txt", ".org")) 
    (ido-mode t) 
     
    ;; with-editor 
    (when (require 'with-editor nil 'noerror) 
      (add-hook 'eshell-mode-hook 'with-editor-export-editor) 
      (define-key (current-global-map) 
        [remap async-shell-command] 'with-editor-async-shell-command) 
      (define-key (current-global-map) 
        [remap shell-command] 'with-editor-shell-command) 
      ;; with-editor for crontab 
      (defun crontab-e () 
        (interactive) 
        ;; requires with-editor package 
        (with-editor-async-shell-command "crontab -e"))) 
     
     
    ;; ein 
    (setq ein:notebook-modes '(ein:notebook-multilang-mode)) 
    (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
    (setq ein:jupyter-default-server-command "jupyter")
    (setq ein:jupyter-default-notebook-directory "b:")
    (setq ein:jupyter-server-args (list "--no-browser"))


    ;; magit
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
    (setq magit-auto-revert-mode nil)

    ;; eshell git
    ;; (eshell-git-prompt-use-theme 'powerline)

    ;; ipython for run-python
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
)) 
 
;; Only load these things if we're not in a terminal 
(if (display-graphic-p) 
    (setup-all-packages)) 
;; end package management 
 
;; custom commands 
(defun swap-buffers () 
  "Move the current buffer into the next window, and vice versa." 
  (interactive) 
  (let* ((this (selected-window)) 
		 (other (next-window)) 
		 (this-buffer (window-buffer this)) 
		 (other-buffer (window-buffer other))) 
	(set-window-buffer other this-buffer) 
	(set-window-buffer this other-buffer))) 
 
;; (defun promote-buffer () 
;;   (interactive) 
;;   (let* ((this (selected-window)) 
;; 		 (other (next-window)) 
;; 		 (this-buffer (window-buffer this))) 
;; 	(set-window-buffer other this-buffer) 
;; 	(set-window-buffer this previous-buffer))) 
 
;; use default C-l binding to clear buffer 
;; in eshell 
(defun eshell-clear-buffer() 
  "Clear terminal" 
  (interactive) 
  (let ((inhibit-read-only t)) 
	(erase-buffer) 
	(eshell-send-input))) 
 
(add-hook 'eshell-mode-hook 
          '(lambda() 
             (local-set-key (kbd "C-l") 'eshell-clear-buffer))) 
 
;; eshell aliases 
(defun eshell/emacs (file) 
  (find-file file)) 
(defun eshell/open (file) 
  (find-file-other-window file)) 
(defun eshell/xb () 
  (ido-switch-buffer)) 
(defun eshell/xo () 
  (other-window 1)) 
 
;; move to middle of current line 
(defun middle-of-line () 
  "Put cursor at middle point of the line." 
  (interactive) 
  (goto-char (/ (+ (point-at-bol) (point-at-eol)) 2))) 
(global-set-key "\M-g\M-l" 'middle-of-line) 
 
(defun go-to-column (column) 
  (interactive "nColumn: ") 
  (move-to-column column t))  
 
;; unset system proxy, for connect with ein 
;; leaves https proxy, for connect with package 
(setenv "http_proxy" "") 

;; i hate the goddamn bell
(setq ring-bell-function 'ignore)

;; if mac/linux, load path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; when on mac, use command key as meta and option as alt
;; also, any PATH edits duplicated from .bashrc (need to figure this out later)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))





;; when on mac, use command key as meta and option as alt
;; also, any PATH edits duplicated from .bashrc (need to figure this out later)

; (add-to-list 'exec-path "/Users/apope/bin")
