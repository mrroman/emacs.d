;;; company-tern --- Enable company-tern -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

(require 'company)

(add-hook 'js2-mode-hook (lambda ()
                           (progn
                             (add-to-list 'company-backends 'company-tern)
                             (tern-mode))))

(add-to-list 'auto-mode-alist '(".tern-project" . js-mode))

;;; company-tern.el ends here
