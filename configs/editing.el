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

(custom-set-faces
 '(whitespace-empty ((t (:background "#e0211d")))))

(global-whitespace-mode)

;; org mode

(case system-type
  (gnu/linux (setq org-default-notes-file "~/Dokumenty/organizer.org"))
  (darwin (setq org-default-notes-file "~/Documents/organizer.org"))
  (t (setq org-default-notes-file "~/organizer.org")))


(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f2>") (lambda ()
                               (interactive)
                               (find-file org-default-notes-file)))
