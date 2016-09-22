;; Projectile

(projectile-global-mode)

(when (equalp 'windows-nt system-type)
  (setq projectile-indexing-method 'alien))

(global-set-key (kbd "<f9>") 'projectile-test-project)
(global-set-key (kbd "<C-f9>") 'projectile-compile-project)
