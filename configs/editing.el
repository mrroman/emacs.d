;;; editing --- Editing tweaks -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

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

(custom-set-faces
 '(whitespace-empty ((t (:background "#e0211d")))))

(global-whitespace-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; editing.el ends here
