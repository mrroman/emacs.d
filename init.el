;;;
;;; Healthy defaults :)
;;;

(setq inhibit-startup-screen t
      initial-scratch-message nil

      create-lockfiles nil
      make-backup-files nil


      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.5
      use-package-always-ensure t
      sentence-end-double-space nil
      help-window-select t
      custom-file "~/.emacs.d/custom.el")

(load-theme 'wombat)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

(electric-indent-mode t)
(eldoc-mode)

(delete-selection-mode t)

(global-set-key (kbd "C-x C-j") 'dired-jump)

;;;
;;; Extensions
;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Code completion

(use-package company
  :init
  (setq company-idle-delay 0.1)
  :config
  (global-company-mode))

;; Lisp editing helpers

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-h b" . counsel-descbinds)))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-mode))

(use-package smart-mode-line
  :config
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package ace-jump-mode
  :bind (("C-." . ace-jump-mode)))

(use-package multiple-cursors
  :bind (("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-M->" . mc/skip-to-next-like-this)
         ("C-c C-M-<" . mc/skip-to-previous-like-this)))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package js2-mode
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  (global-whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))
