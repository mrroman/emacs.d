;;; neotree --- neotree configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'neotree)
(require 'projectile)

(defun my/neotree-toggle ()
  "Toggle neotree window.  If buffer is a project buffer, open neotree based on projectile directory."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (progn
      (if (projectile-project-p)
          (progn
            (neotree-find)
            (neotree-dir (projectile-project-root)))
        (neotree-show)))))

(global-set-key (kbd "<C-tab>") 'my/neotree-toggle)

;;; neotree.el ends here
