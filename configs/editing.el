;; Editing tweaks

(delete-selection-mode t)
(whole-line-or-region-mode)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't use arrow keys

(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])

;; Enable whitespace mode

;; whitespace-mode config
(require 'whitespace)

(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(add-hook 'text-mode-hook (lambda ()
			    (whitespace-mode t)))
