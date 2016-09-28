;;; org-mode --- org-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(setq org-return-follows-link t)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f2>") (lambda ()
                               (interactive)
                               (let ((notes-buffer-name (file-name-nondirectory org-default-notes-file)))
                                 (if (get-buffer notes-buffer-name)
                                     (kill-buffer notes-buffer-name)
                                   (find-file org-default-notes-file)))))

(defun my/system-p (system)
  "Check if Emacs is running on system SYSTEM."
  (equal system-type system))

(cond
 ((my/system-p 'gnu/linux) (setq org-default-notes-file "~/Dokumenty/organizer.org"))
 ((my/system-p 'darwin) (setq org-default-notes-file "~/Documents/organizer.org"))
 (t (setq org-default-notes-file "~/organizer.org")))

;;; org-mode.el ends here
