(put 'upcase-region 'disabled nil)
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
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; remove backup files
(setq make-backup-files nil)

;; wrap lines
(global-visual-line-mode 1)


;; for non-terminal-based emacs
(defun load-gui-options ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'tango-dark)
  (set-default-font "Fantasque Sans Mono-13"))

(if (display-graphic-p)
	(load-gui-options))

;; set package manager archives
(require 'package)
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

;; jedi for python mode
(add-hook 'python-mode-hook 'jedi:setup)
;; jedi for ipython mode
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(print window-system)
