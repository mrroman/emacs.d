;; set up packages

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

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

(my/package-required '(color-theme-wombat
		       company
		       helm
		       multiple-cursors
		       ace-jump-mode
		       whole-line-or-region
		       cider
		       smartparens
		       undo-tree
		       magit
		       projectile
		       helm-projectile))

(my/load-config "ui-tweaks")
(my/load-config "editing")

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
