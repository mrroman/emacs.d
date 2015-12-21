;; set up packages

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defun package-required (pkg-list)
  (dolist (pkg-name pkg-list)
    (unless (package-installed-p pkg-name)
      (package-install pkg-name))))

(package-required '(color-theme-wombat
		    company
		    helm
		    multiple-cursors
		    ace-jump-mode
		    whole-line-or-region
		    cider
		    smartparens
		    undo-tree
		    magit))

;; color themes

(require 'color-theme-wombat)
(color-theme-wombat)

;; UI tweaks

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(show-paren-mode)

;; Editing tweaks

(delete-selection-mode t)
(whole-line-or-region-mode)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])

;; Coding tweaks

(global-company-mode t)
(eldoc-mode t)

;; Smart parens
(require 'smartparens-config)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; Helm mode

(helm-mode)

;; Window navigation
(windmove-default-keybindings)

;; Acejump and Multicursors
(global-set-key (kbd "C->") 'ace-jump-mode)
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)
