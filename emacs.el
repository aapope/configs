;; set custom key codes
(put 'upcase-region 'disabled nil)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(add-hook 'python-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;;set custom variables
(custom-set-variables
 '(tramp-default-method "ssh"))
;; add default markdown mode extensions
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  

;; remove backup files because I hate them
(setq make-backup-files nil)

;; wrap lines
(global-visual-line-mode 1)

;; code folding
(if (>= emacs-major-version 24)
    (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'python-mode-hook #'hs-minor-mode)
  )


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

  ;; org-mode agenda files
  (setq org-agenda-files (quote ("/ssh:apope@andrewapope.com:~/orgs")))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "BLOCKED(b@)")
	  (sequence "|" "CANCELED(c@)")
	  (sequence "URGENT(u!)")))
  (setq org-todo-keyword-faces
	'(("TODO" . "orange") ("BLOCKED" . "yellow") ("CANCELED" . "green")
	  ("DONE" . "green") ("URGENT" . "red")))
  
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
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)

  ;; ein
  (setq ein:notebook-modes '(ein:notebook-multilang-mode))
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  )
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

;; move to middle of current line
(defun middle-of-line ()
  "Put cursor at middle point of the line."
  (interactive)
  (goto-char (/ (+ (point-at-bol) (point-at-eol)) 2)))

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(defun crontab-e ()
  (interactive)
  ;; requires with-editor package
  (with-editor-async-shell-command "crontab -e"))

;; unset system proxy, for connect with ein
(setenv "http_proxy" "")
