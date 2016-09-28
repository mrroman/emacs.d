;;; flycheck-clojure --- Enable flycheck-clojure -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; flycheck-clojure.el ends here
