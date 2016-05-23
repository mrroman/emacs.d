(require 'nvm)

(add-hook 'projectile-after-switch-project-hook (lambda ()
                                                  (when (file-exists-p (concat (projectile-project-root) ".nvmrc"))
                                                    (nvm-use-for (projectile-project-root))
                                                    (setq exec-path (remove-if (lambda (x)
                                                                                 (string-match-p "\\.nvm" x)) exec-path))
                                                    (add-to-list 'exec-path
                                                                 (concat (file-name-as-directory (second nvm-current-version))
                                                                         "bin")))))
