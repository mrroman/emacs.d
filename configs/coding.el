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

;; enable electric pair mode

(electric-pair-mode t)

;; auto select help window
(setq help-window-select t)

;; colorize compilation buffer
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-process-output nil)
    (setq-local comint-last-output-start (point-marker))))

(setq compilation-scroll-output t)
(add-hook 'compilation-filter-hook #'endless/colorize-compilation)
