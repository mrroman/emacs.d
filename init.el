;; set up packages

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;; load package archives if no packages installed

(let ((elpa-dir (concat (file-name-as-directory user-emacs-directory) "elpa/")))
  (when (not (file-exists-p elpa-dir))
    (package-refresh-contents)))

(package-initialize)

(defun my/install-package (pkg-name)
  (unless (package-installed-p pkg-name)
    (package-install pkg-name)))

(defvar my/configs-directory 
  (concat (file-name-as-directory user-emacs-directory) "configs/"))

(defun my/load-config (name)
  (let ((file-with-path (concat my/configs-directory name ".el")))
    (when (file-exists-p file-with-path)
      (load-file file-with-path))))

(defun my/package-required (pkg-list)
  (dolist (pkg-name pkg-list)
    (my/install-package pkg-name)
    (my/load-config (symbol-name pkg-name))))

(my/package-required '(f
		       color-theme-wombat
		       spacemacs-theme
		       company
		       helm
		       multiple-cursors
		       ace-jump-mode
		       whole-line-or-region
		       cider
		       clj-refactor
		       clojure-mode-extra-font-locking
		       smartparens
		       undo-tree
		       magit
		       projectile
		       helm-projectile
		       expand-region
		       helm-descbinds
		       helm-ag
		       helm-google))

(my/load-config "ui-tweaks")
(my/load-config "editing")

(require 'f)
(setq custom-file "~/.emacs.d/custom.el")
(f-touch custom-file)
(load custom-file)

;; Coding tweaks

(global-company-mode t)

;; Window navigation
(windmove-default-keybindings)

;; Acejump and Multicursors
(global-set-key (kbd "C->") 'ace-jump-mode)
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)
