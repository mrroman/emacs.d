;;;
;;; Healthy defaults :)
;;;

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

(electric-indent-mode t)
(electric-pair-mode t)
(eldoc-mode)
(show-paren-mode)

(delete-selection-mode t)
(windmove-default-keybindings)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(message "Loading extensions...")

;;;
;;; Extensions
;;;

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
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Init

(message "Loading extensions: Init")

(defun my/system-p (system)
  "Check if Emacs is running on system SYSTEM."
  (equal system-type system))

(use-package exec-path-from-shell
  :if (not (my/system-p 'windows-nt))
  :config
  (add-to-list 'exec-path-from-shell-variables "GOROOT")
  (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing

(message "Loading extensions: Editing")

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode))

(use-package ace-jump-mode
  :bind (("C-." . ace-jump-mode)))

(use-package multiple-cursors
  :bind (("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-M->" . mc/skip-to-next-like-this)
         ("C-c C-M-<" . mc/skip-to-previous-like-this)))

(use-package whitespace
  :ensure nil
  :diminish global-whitespace-mode
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  (global-whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-mode)
  :init
  (global-undo-tree-mode))

(use-package expand-region
  :bind ("M-SPC" . er/expand-region))

(when (my/system-p 'darwin)
  (setq ns-right-alternate-modifier nil))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package winum
  :init
  (winum-mode))

(use-package smex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI

(message "Loading extensions: UI")

(use-package kaolin-theme
  :ensure t
  :config
  (load-theme 'kaolin t))

(use-package telephone-line
  :if window-system
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode t))

(when window-system
  (use-package beacon
    :diminish beacon-mode
    :config
    (beacon-mode t)))

(defun my/set-font (name)
  "Try to set font for frame."
  (if (find-font (font-spec :name name))
      (set-frame-font name)
    (message "Font '%s' not found" name)))

(when window-system
  (cond
   ((my/system-p 'darwin) (progn
                            (my/set-font "Iosevka Term 15")
                            (set-frame-size (selected-frame) 170 42)))
   ((my/system-p 'windows-nt) (progn
                                (my/set-font "Iosevka Term 11")
                                (set-frame-size (selected-frame) 140 40)))
   ((my/system-p 'gnu/linux) (my/set-font "Iosevka Term 13"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Terminal

(message "Loading extensions: Terminal")

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill buffer and window on term exit."
  (kill-buffer-and-window))

(defvar my/term-number 0)

(require 'term)

(defun my/show-term ()
  (interactive)
  (let* ((window (split-window-below -10))
         (buf-name (format "ansi-term<%d>" (setq my/term-number (+ my/term-number 1))))
         (buffer (term-ansi-make-term buf-name (getenv "SHELL"))))
    (progn
      (set-buffer buffer)
      (term-mode)
      (term-char-mode)
      (whitespace-mode 0)
      (let (term-escape-char)
        ;; I wanna have find-file on C-x C-f -mm
        ;; your mileage may definitely vary, maybe it's better to put this in your
        ;; .emacs ...
        (term-set-escape-char ?\C-x))
      (set-window-buffer window buffer)
      (select-window window))))

(global-set-key (kbd "C-c t") 'my/show-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coding

(message "Loading extensions: Coding")

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1)
  :config
  (global-company-mode))

(use-package pos-tip)

(use-package company-quickhelp
  :if window-system
  :config
  (company-quickhelp-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(use-package yasnippet
  :bind (("C-c y" . company-yasnippet))
  :config
  (yas-global-mode 1))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

;;; Colorize compilation buffer

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-process-output nil)
    (setq-local comint-last-output-start (point-marker))))

(setq-default compilation-scroll-output t)
(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy

(message "Loading extensions: Ivy")

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-hydra)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-h b" . counsel-descbinds)))

(use-package swiper
  :bind (("C-s" . swiper))
  :config
  (define-key swiper-map (kbd "C-.")
    (lambda ()
      (interactive)
      (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projects

(message "Loading extensions: Projects")

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-toggle 1))

(use-package ag)

(use-package dired-x
  :ensure nil
  :bind ("C-x C-j" . dired-jump))

;; (use-package neotree
;;   :config
;;   (defun my/neotree-toggle ()
;;     "Toggle neotree window.  If buffer is a project buffer, open neotree based on projectile directory."
;;     (interactive)
;;     (if (neo-global--window-exists-p)
;;         (neotree-hide)
;;       (progn
;;         (if (projectile-project-p)
;;             (progn
;;               (neotree-find)
;;               (neotree-dir (projectile-project-root)))
;;           (neotree-show)))))

;;   (global-set-key (kbd "<C-tab>") 'my/neotree-toggle))

(use-package ztree
  :bind (("C-x C-d" . ztree-dir)))

(global-set-key (kbd "<C-tab>") (lambda ()
                                  (interactive)
                                  (if (projectile-project-p)
                                      (ztree-dir (projectile-project-root))
                                    (ztree-dir default-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Web

(message "Loading extensions: Web")

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsp?\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Javascript

(message "Loading extensions: Javascript")

(use-package js2-mode
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package company-tern
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (progn
                               (add-to-list 'company-backends 'company-tern)
                               (tern-mode))))
  (add-to-list 'auto-mode-alist '(".tern-project" . js-mode)))

(use-package nvm
  :config
  (defun my/setup-node-using-nvm ()
    "Set up correct node version using nvm."
    (when (and
           (projectile-project-p)
           (file-exists-p (concat (projectile-project-root) ".nvmrc")))
      (nvm-use-for (projectile-project-root))

      (delq (car (delq nil
                       (mapcar
                        (lambda (x) (when (string-match-p "\\.nvm" x) x))
                        exec-path)))
            exec-path)

      (add-to-list 'exec-path
                   (concat (file-name-as-directory (cadr nvm-current-version))
                           "bin"))))

  (add-hook 'projectile-after-switch-project-hook #'my/setup-node-using-nvm))

(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert"
                                   "refute" "setTimeout" "clearTimeout"
                                   "setInterval" "clearInterval" "location"
                                   "__dirname" "console" "JSON" "it" "describe"
                                   "beforeEach" "afterEach" "before" "after"))

(defun my/custom-js-indent ()
  "Set up custom indent for Javascript package.json."
  (when-let ((buffer-file-path (buffer-file-name (current-buffer))))
    (let ((fname (file-name-nondirectory buffer-file-path)))
      (when (string= fname "package.json")
        (setq-local js-indent-level 2)))))

(defun my/npm-dep-bin-path (name)
  "Return path to npm deps NAME binary."
  (projectile-expand-root (concat "node_modules/.bin/" name)))

(defun my/js-linters-setup ()
  "Look for jslinters in project devdependencies and use them if declared."
  (when (projectile-project-p)
    (let ((package-json-file (projectile-expand-root "package.json")))
      (when (file-exists-p package-json-file)
        (let* ((package-json (json-read-file package-json-file))
               (dev-deps (alist-get 'devDependencies package-json)))
          (cond ((alist-get 'eslint dev-deps)
                 (setq-local flycheck-javascript-eslint-executable (my/npm-dep-bin-path "eslint")))
                ((alist-get 'jshint dev-deps)
                 (setq-local flycheck-javascript-jshint-executable (my/npm-dep-bin-path "jshint")))))))))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(add-hook 'js-mode-hook 'my/custom-js-indent)
(add-hook 'js-mode-hook 'my/js-linters-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Java

(use-package meghanada
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python

(message "Loading extensions: Python")

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (anaconda-mode 1)
                                (anaconda-eldoc-mode 1))))

(use-package company-anaconda
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'company-backends 'company-anaconda))))

(use-package pyenv-mode
  :config
  (defun my/projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project-dir (locate-dominating-file "." ".python-version")))
      (if project-dir
          (pyenv-mode-set project-dir)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'my/projectile-pyenv-mode-set))

(use-package nose
  :bind (:map python-mode-map
              ("C-c t a" . nosetests-all)
              ("C-c t m" . nosetests-module)
              ("C-c t t" . nosetests-again)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Java

(defun my/java-mode-hook ()
  (semantic-mode)
  (set (make-local-variable 'company-backends)
       '((company-semantic company-dabbrev company-yasnippet)))
  (setq-local company-dabbrev-downcase nil))

(add-hook 'java-mode-hook #'my/java-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure

(message "Loading extensions: Clojure")

(use-package cider
  :bind (("TAB" . company-indent-or-complete-common)
         :map clojure-mode-map
         ("C-c C-j" . imenu))
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (yas-minor-mode 1))))

(use-package clj-refactor
  :config
  (defun my/clj-refactor-init ()
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "M-RET"))
  (add-hook 'clojure-mode-hook #'my/clj-refactor-init))

(use-package flycheck-joker
  :config
  (require 'flycheck-joker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lisp editing helpers

(message "Loading extensions: Lisp")

(use-package paredit
  :commands (paredit-mode))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojurescript-mode-hook #'paredit-mode)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojurescript-mode-hook #'aggressive-indent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C/C++

(message "Loading extensions: C/C++")

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Go

(message "Loading extensions: Go")

(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :bind (:map go-mode-map
              ("C-c d" . godoc-at-point)
              ("M-." . godef-jump))
  :config
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))
  (add-hook 'go-mode-hook (lambda ()
                            (go-eldoc-setup)
                            (subword-mode +1)
                            (setq-local tab-width 8)
                            (setq-local indent-tabs-mode 1)
                            (setq-local whitespace-style '(face empty trailing lines-tail))
                            (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package go-eldoc
  :mode ("\\.go$" . go-mode))
(use-package go-guru
  :mode ("\\.go$" . go-mode))
(use-package go-rename
  :mode ("\\.go$" . go-mode))

(use-package gotest
  :mode ("\\.go$" . go-mode)
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c ." . go-test-current-test)
              ("C-c b" . go-run)))

(use-package company-go
  :mode ("\\.go$" . go-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scala

(message "Loading extensions: Scala")

(use-package ensime
  :ensure t
  :commands (ensime)
  :pin melpa
  :config
  (setq ensime-startup-notification nil
        ensime-startup-snapshot-notification nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CommonLisp

(message "Loading extensions: CommonLisp")

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl --noinform --no-linedit")
  (add-to-list 'slime-contribs 'slime-fancy)
  (slime-setup))

(use-package slime-company
  :init
  (add-to-list 'slime-contribs 'slime-company)
  (slime-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode

(message "Loading extensions: Org-mode")

(use-package org
  :ensure org
  :pin org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (progn
    (when (file-exists-p "~/Dropbox/Notatki")
      (setq org-directory "~/Dropbox/Notatki"))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-agenda-files (list org-directory))
    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 3)))))

(use-package org-bullets
  :config
  (org-bullets-mode)
  (add-hook 'org-mode-hook #'org-bullets-mode))

(setq calendar-week-start-day 1
      calendar-day-name-array ["Niedziela" "Poniedziałek" "Wtorek" "Środa"
                               "Czwartek" "Piątek" "Sobota"]
      calendar-month-name-array ["Styczeń" "Luty" "Marzec" "Kwiecień" "Maj"
                                 "Czerwiec" "Lipiec" "Sierpień" "Wrzesień"
                                 "Październik" "Listopad" "Grudzień"])

(use-package ox-reveal
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(require 'ob-java)
(require 'ob-python)
(require 'ob-clojure)
(require 'ob-shell)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (clojure . t)
   (java . t)
   (shell . t)))

(setq-default major-mode 'org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YAML Mode

(message "Loading extensions: YAML")

(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RestClient

(use-package restclient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MPD client

(message "Configure MPD client")

(use-package mpc
  :ensure nil
  :bind (:map mpc-mode-map
              ("M-RET" . mpc-play-at-point)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
