;; UI tweaks

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(show-paren-mode)

;; color themes

;; (require 'color-theme-wombat)
;; (color-theme-wombat)
(load-theme 'spacemacs-dark t)

;; (global-hl-line-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; font size

(case system-type
  (darwin (set-frame-font "Fira Code 14"))
  (gnu/linux (set-frame-font "Iosevka 13")))
