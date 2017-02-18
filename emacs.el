;; set custom key codes
(put 'upcase-region 'disabled nil)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(setq-default indent-tabs-mode nil)

;; python settings
(add-hook 'python-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; update SQL mode to open .prc
(add-to-list 'auto-mode-alist '("\\.prc\\'" . sql-mode))

;;set custom variables
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (custom-set-variables
     '(tramp-default-method "plink"))
  (custom-set-variables
   '(tramp-default-method "ssh")))

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


;; for non-terminal-based emacs:
;; remove tool bars, set theme and font,
;; and set background transparency
(defun load-gui-settings ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'tango-dark)
  (set-default-font "Fantasque Sans Mono-13")
  ;; semi-transparent background
  (set-frame-parameter (selected-frame) 'alpha '(85 85))
  (add-to-list 'default-frame-alist '(alpha 85 85)))

(if (display-graphic-p)
	(load-gui-settings))


;; if we have a package manager, install default
;; packages and customize
(defun setup-all-packages ()
  (when (require 'package nil 'noerror)
    ;; set package manager archives
    (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			     ("marmalade" . "https://marmalade-repo.org/packages/")
			     ("melpa" . "http://melpa.org/packages/")
			     ("org" . "http://orgmode.org/elpa/")))
    (package-initialize)
    
    
    ;; checks list of required packages, installs if not available
    (require 'cl)
    (defvar prelude-packages
      '(org jedi jedi-core ein with-editor ess ido)
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

    ;; org-mode agenda files
    ;; Set protocol based on OS type
    ;; Only setup function, make emacs ask.
    (defun org-load-files ()
      (interactive)
      (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
          (setq org-agenda-files (quote ("/plink:apope@andrewapope.com:~/orgs")))
        (setq org-agenda-files (quote ("/ssh:apope@andrewapope.com:~/orgs")))))
    
    ;; org-mode agenda files
    (setq org-agenda-files (quote ("/ssh:apope@andrewapope.com:~/orgs")))
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "|" "DONE(d!/!)")
	    (sequence "BLOCKED(b@/!)")
	    (sequence "|" "CANCELED(c@)")
	    (sequence "URGENT(u)")
            (sequence "IN-QA(q/@)")
            (sequence "DO-QA(a)" "|" "DONE(d!/!)")))
    (setq org-todo-keyword-faces
	  '(("TODO" . "orange") ("BLOCKED" . "yellow") ("CANCELED" . "green")
	    ("DONE" . "green") ("URGENT" . "red") ("IN-QA" . "yellow")
	    ("DO-QA" . "orange")))


    ;;; org-outlook.el - Support for links to Outlook items in Org
    ;;; To find the corresponding Outlook macro, go to:
    ;;; https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
    ;; (org-add-link-type "outlook" 'org-outlook-open)
    (defun org-outlook-open (id)
      "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
      (w32-shell-execute "open" "C:/Program Files/Microsoft Office/Office15/OUTLOOK.EXE" (concat "/select " "outlook:" id)))
    (provide 'org-outlook)
    
    ;; enable python for in-buffer evaluation
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (java . t)))
    
    ;; use orgstruct++ minor mode in python code
    ;; useful for extended documentation sections
    ;; if emacs major mode is >= 24, turn on for all
    ;; programming modes
    (if (>= emacs-major-version 24)
	(add-hook 'prog-mode-hook #'orgstruct++-mode)
      (add-hook 'python-mode-hook #'orgstruct++-mode))
    
    ;; jedi for python mode
    (add-hook 'python-mode-hook 'jedi:setup)
    ;; jedi for ipython mode
    (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
    
    ;; ido mode
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq ido-file-extension-order '(".py", ".txt", ".org"))
    (ido-mode t)
    
    ;; with-editor
    (require 'with-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)
    (define-key (current-global-map)
      [remap async-shell-command] 'with-editor-async-shell-command)
    (define-key (current-global-map)
      [remap shell-command] 'with-editor-shell-command)
    ;; with-editor for crontab
    (defun crontab-e ()
      (interactive)
      ;; requires with-editor package
      (with-editor-async-shell-command "crontab -e"))
    
    
    ;; ein
    (setq ein:notebook-modes '(ein:notebook-multilang-mode))
    (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
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
