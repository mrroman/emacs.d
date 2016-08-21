(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "it" "describe" "beforeEach" "afterEach" "before" "after"))

