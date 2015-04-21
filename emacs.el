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
(global-set-key "\C-h" 'backward-delete-char)
(add-to-list 'load-path "/home/apope/.emacs.d/auto-save-list")
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(setq make-backup-files nil)
(global-visual-line-mode 1)

;; add newer version of org-mode
;; (add-to-list 'load-path "~/.emacs.d/org-8.2.5h/lisp")
;; (add-to-list 'load-path "~/.emacs.d/org-8.2.5h/contrib/lisp")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; enable python for in-buffer evaluation
(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (java . t)))

;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
;; (setq pylookup-dir "~/.emacsd/pylookup-master")
;; (add-to-list 'load-path pylookup-dir)

;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; set search option if you want
;; ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup" 
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; (require 'w3m-load)
;; (require 'w3m)
;;  (setq browse-url-browser-function 'browse-url-generic
;;        browse-url-generic-program "/usr/bin/conkeror")

;; (defun choose-browser (url &rest args)
;;   (interactive "sURL: ")
;;   (if (y-or-n-p "Use external browser? ")
;;       (w3m-browse-url url)
;;     (w3m-browse-url url)))

;; (setq browse-url-browser-function 'choose-browser)
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; use theme
(load-theme 'tango-dark)

;; package manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "https://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.org/packages/")
						 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
	  
;; jedi for python mode
(add-hook 'python-mode-hook 'jedi:setup)
;; jedi for ipython mode
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; change the default font
(set-default-font "Fantasque Sans Mono-13")
