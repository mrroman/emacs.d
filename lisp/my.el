;;; my -- my custom functions
;;;
;;; Commentary:
;;; Code:

(defun my/system-p (system)
  "Check if Emacs is running on system SYSTEM."
  (equal system-type system))

(defun my/set-font (name)
  "Try to set font NAME for the frame."
  (if (find-font (font-spec :name name))
      (set-frame-font name nil t)
    (message "Font '%s' not found" name)))

(defun my/music ()
  (interactive)
  (make-process :name "mpg123"
                :buffer "*mpg123*"
                :command (list "mpg123" (read-file-name "Music file: " (expand-file-name "~/Music/")))
                :connection-type 'pty)
  (with-current-buffer "*mpg123*"
    (ansi-term)
    (term-char-mode)))



(provide 'my)

;;; my.el ends here
