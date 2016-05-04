;;; init --- Configuration and packages setup
;;;
;;; Commentary:

;; set up packages

(require 'package)

;;; Code:

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
  "Install package PKG-NAME unless it is not already installed."
  (unless (package-installed-p pkg-name)
    (package-install pkg-name)))

(defvar my/configs-directory
  (concat (file-name-as-directory user-emacs-directory) "configs/")
  "Directory with configuration files of installed packages.")

(defun my/load-config (name)
  "Load configuration for package NAME from configuration directory."
  (let ((file-with-path (concat my/configs-directory name ".el")))
    (when (file-exists-p file-with-path)
      (load-file file-with-path))))

(defun my/package-required (pkg-list)
  "Install and configure all packages on PKG-LIST."
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
               flycheck-clojure

               flycheck-pos-tip
		       smartparens
		       undo-tree
		       magit

		       projectile
		       helm-projectile
		       expand-region
		       helm-descbinds
		       helm-ag
		       helm-google

		       go-mode
		       go-eldoc
		       company-go
               go-projectile
               gotest

               cmake-ide
               company-c-headers

               smart-mode-line
               beacon
               ace-window
               aggressive-indent))

(my/load-config "ui-tweaks")
(my/load-config "editing")
(my/load-config "coding")
(my/load-config "keybindings")

(my/load-config "golang")
(if (equalp system-type 'gnu/linux)
    (my/load-config "linux"))

(my/load-config "custom-file")

(provide 'init)
;; init ends here
