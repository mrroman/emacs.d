;;; clj-refactor --- Enable clj-refactor -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Code:

(require 'clj-refactor)

(defun my/clojure-mode-hook ()
  "Enable clj refactor in Clojure buffers."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "M-RET"))

(add-hook 'clojure-mode-hook #'my/clojure-mode-hook)

;;; clj-refactor.el ends here
