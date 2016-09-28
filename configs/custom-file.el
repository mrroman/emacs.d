;;; custom-file --- Enable custom-file -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

;; Select other custom-file

(setq custom-file (concat (file-name-as-directory user-emacs-directory) "custom.el"))

(when (not (file-exists-p custom-file)) ;; windows emacs bug (can't load file if it's empty) workaround
  (write-region ";; Customize file\n" nil custom-file))

(load-file custom-file)

;;; custom-file.el ends here
