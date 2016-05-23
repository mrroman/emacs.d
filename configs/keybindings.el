;; Window navigation
(global-set-key (kbd "C-x o") 'ace-window)

;; Acejump and Multicursors
(global-set-key (kbd "C->") 'ace-jump-mode)
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

;; Terminal

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer-and-window))

(defvar my/term-number 0)

(require 'term)

(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (let* ((window (split-window-below -10))
                                       (buf-name (format "ansi-term<%d>" (incf my/term-number)))
                                       (buffer (term-ansi-make-term buf-name (getenv "SHELL"))))
                                  (progn
                                    (set-buffer buffer)
                                    (term-mode)
                                    (term-char-mode)
                                    (whitespace-mode 0)
                                    (let (term-escape-char)
                                      ;; I wanna have find-file on C-x C-f -mm
                                      ;; your mileage may definitely vary, maybe it's better to put this in your
                                      ;; .emacs ...
                                      (term-set-escape-char ?\C-x))
                                    (set-window-buffer window buffer)
                                    (select-window window)))))

;; Turn of annoying keyscores

(global-unset-key (kbd "C-<previous>"))
(global-unset-key (kbd "C-<next>"))
