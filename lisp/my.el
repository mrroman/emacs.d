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
      (set-frame-font name)
    (message "Font '%s' not found" name)))

(provide 'my)

;;; my.el ends here
