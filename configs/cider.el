;; cider - help banner

(setq cider-repl-display-help-banner nil)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojurescript-mode #'paredit-mode)
