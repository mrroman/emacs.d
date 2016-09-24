;;; projectile.el --- org-mode settings -*- lexical-binding: t -*-
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

;;; org-mode.el ends here
