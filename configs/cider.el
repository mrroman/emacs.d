;;; cider --- Set up CIDER -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

;; cider - help banner

(require 'cider)
(require 'company)

(setq cider-repl-display-help-banner nil)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojurescript-mode #'paredit-mode)

;;; cider.el ends here
