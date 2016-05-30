;; Coding tweaks

(global-company-mode t)
(setq company-idle-delay 0.1)

;; eldoc (documentation of current function)

(eldoc-mode)

;; don't use tabs

(require 'whitespace)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; enable paredit for Lisp

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
