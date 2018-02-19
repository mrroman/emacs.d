(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'my)

(setq inhibit-startup-screen t
      initial-scratch-message nil

      create-lockfiles nil
      make-backup-files nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.5

      sentence-end-double-space nil
      dired-dwim-target t
      help-window-select t
      custom-file (expand-file-name "~/.emacs.d/custom.el"))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

(when (my/system-p 'darwin)
  (add-to-list 'exec-path "/usr/local/bin")
  (setenv "PATH" (format "%s:/usr/local/bin" (getenv "PATH"))))

(electric-indent-mode t)
(electric-pair-mode t)
(eldoc-mode)
(show-paren-mode)

(delete-selection-mode t)
(windmove-default-keybindings)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(setq dired-omit-files (concat dired-omit-files "\\|^\\.[^\\.]"))

(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x C-j") #'dired-jump)

;;; UI

(when window-system
  (load-theme 'wombat)
  (cond
   ((my/system-p 'darwin) (progn
                            (my/set-font "Iosevka Term 16")
                            (set-frame-size (selected-frame) 170 42)))
   ((my/system-p 'windows-nt) (progn
                                (my/set-font "Iosevka Term 11")
                                (set-frame-size (selected-frame) 140 40)))
   ((my/system-p 'gnu/linux) (my/set-font "Iosevka Term 13"))))

;;;
;;; Extensions
;;;

;;;; Init

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

;;;; UI

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package diminish)
(use-package delight
  :pin gnu)

;;;; Editing

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-char-mode))

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode))

(use-package undo-tree
  :pin gnu
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(use-package expand-region
  :bind ("M-SPC" . er/expand-region))

(when (my/system-p 'darwin)
  (setq ns-right-alternate-modifier nil))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package smex)

(use-package multiple-cursors
  :bind (("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-M->" . mc/skip-to-next-like-this)
         ("C-c C-M-<" . mc/skip-to-previous-like-this)))

;;;; Ivy - Counsel

(use-package ivy
  :diminish ivy-mode
  :pin melpa
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-hydra
  :pin melpa)

(use-package counsel
  :pin melpa
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-h b" . counsel-descbinds)))

(use-package swiper
  :bind (("C-s" . swiper))
  :pin melpa
  :config
  (define-key swiper-map (kbd "C-.")
    (lambda ()
      (interactive)
      (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol)))))))

;;;; Projects

(message "Loading extensions: Projects")

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package projectile
  :ensure projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (projectile-global-mode t)))

(use-package ag)

;;;; Coding

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1)
  (global-company-mode))

(use-package pos-tip)

(use-package company-quickhelp
  :if window-system
  :config
  (company-quickhelp-mode 1))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-process-output nil)
    (setq-local comint-last-output-start (point-marker))))

(setq-default compilation-scroll-output t)
(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

;;;; Clojure

(use-package cider
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :bind (("TAB" . company-indent-or-complete-common)
         :map clojure-mode-map
         ("C-c C-j" . imenu))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-refresh-before-fn "mount.core/stop"
        cider-refresh-after-fn "mount.core/start")
  (add-hook 'clojure-mode-hook (lambda ()
                                 (yas-minor-mode 1))))

(use-package clj-refactor
  :commands (clj-refactor-mode)
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "M-RET"))))

(use-package flycheck-joker
  :config
  (require 'flycheck-joker))

(require 'cider-cli)

;;;; Lisp

(use-package parinfer
  :config
  (add-hook 'lisp-mode-hook (lambda () (parinfer-mode)))
  (add-hook 'clojure-mode-hook (lambda () (parinfer-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YAML Mode

(message "Loading extensions: YAML")

(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RestClient

(use-package restclient
  :pin melpa
  :commands (restclient-mode))

(use-package ob-restclient
  :pin melpa)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;;; Docker file

(use-package dockerfile-mode
  :pin melpa)

(use-package powershell)

;;; Javascript

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-mode)
  :config
  (add-hook 'javascript-mode #'js2-mode))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "M-RET"))

(use-package xref-js2
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package tern
  :pin melpa
  :config
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :pin melpa
  :config
  (add-hook 'js2-mode-hook #'(lambda ()
                               (add-to-list 'company-backends 'company-tern))))

(use-package nvm
  :pin melpa
  :config
  (add-hook 'projectile-after-switch-project-hook #'(lambda ()
                                                      (when (file-exists-p (expand-file-name ".nvmrc" (projectile-project-root)))
                                                        (nvm-use-for (projectile-project-root))))))


;; Terminal

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(global-set-key (kbd "C-c t") #'(lambda ()
                                  (interactive)
                                  (ansi-term (getenv "SHELL"))))

;; Music player

(use-package emms
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (require 'emms-mode-line)
  (emms-mode-line 1))

;; Start server

(require 'server)
(unless (server-running-p)
    (server-start))

