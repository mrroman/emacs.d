;;; nvm --- Enable nvm -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

(require 'nvm)
(require 'projectile)

(defun my/setup-node-using-nvm ()
  "Set up correct node version using nvm."
  (when (file-exists-p (concat (projectile-project-root) ".nvmrc"))
    (nvm-use-for (projectile-project-root))
    (setq exec-path (remove-if (lambda (x)
                                 (string-match-p "\\.nvm" x)) exec-path))
    (add-to-list 'exec-path
                 (concat (file-name-as-directory (second nvm-current-version))
                         "bin"))))

(add-hook 'projectile-after-switch-project-hook #'my/setup-node-using-nvm)

;;; nvm.el ends here
