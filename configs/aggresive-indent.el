;;; aggresive-indent --- Enable aggresive indents in few modes -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

(require 'aggressive-indent)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;;; aggresive-indent.el ends here
