(add-hook 'js2-mode-hook (lambda ()
                           (progn
                             (add-to-list 'company-backends 'company-tern)
                             (tern-mode))))
