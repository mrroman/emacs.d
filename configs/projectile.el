;; Projectile

(projectile-global-mode)

(when (equalp 'windows-nt system-type)
  (setq projectile-indexing-method 'alien))
