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
		       helm-projectile
		       expand-region))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region helm-projectile projectile magit undo-tree smartparens cider whole-line-or-region ace-jump-mode multiple-cursors helm company color-theme-wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
