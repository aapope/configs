(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tab-width 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; set custom key codes
(put 'upcase-region 'disabled nil)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

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
	'(org jedi jedi-core ein)
	"A list of packages that need to be installed at launch.")
  
  (defun prelude-packages-installed-p ()
	(loop for p in prelude-packages
		  when (not (package-installed-p p)) do (return nil)
		  finally (return t)))
  
  (unless (prelude-packages-installed-p)
	;; check for new packages (packages versions)
	(message "%s" "Emacs is now refreshing package database...")
	(package-refresh-contents)
	(message "%s" "done")
	;; install missing packages
	(dolist (p prelude-packages)
	(when (not (package-installed-p p))
	  (package-install p))))

  (provide 'prelude-packages)
  ;; end package installs
  
  ;; custom keys for org-mode
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
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
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup))
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
