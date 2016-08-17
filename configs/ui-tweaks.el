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
;; (load-theme 'wombat t)

;; (load-theme 'spacemacs-dark t)
;; (load-theme 'tango-plus t)
;; (load-theme 'minimal-light t)
(require 'moe-theme)

(setq moe-theme-highlight-buffer-id t)
(moe-theme-set-color 'cyan)

(moe-light)

;; (load-theme 'leuven t)

;; (global-hl-line-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; font size

(case system-type
  (darwin (set-frame-font "Fira Code 14"))
  (windows-nt (set-frame-font "Iosevka Term 11"))
  (gnu/linux (set-frame-font "Iosevka Term 13")))

;; translucency

;; (add-to-list 'default-frame-alist '(alpha . (100 . 75)))

