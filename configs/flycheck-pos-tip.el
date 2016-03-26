;; Enable flycheck-pos-tip

(require 'flycheck-pos-tip)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
