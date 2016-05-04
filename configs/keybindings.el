;; Window navigation
(global-set-key (kbd "C-x o") 'ace-window)

;; Acejump and Multicursors
(global-set-key (kbd "C->") 'ace-jump-mode)
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

;; Terminal

(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (ansi-term (getenv "SHELL"))))

;; Turn of annoying keyscores

(global-unset-key (kbd "C-<previous>"))
(global-unset-key (kbd "C-<next>"))
