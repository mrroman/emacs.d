;; Coding tweaks

(global-company-mode t)
(setq company-idle-delay 0.1)

;; eldoc (documentation of current function)

(eldoc-mode)

;; don't use tabs

(require 'whitespace)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
