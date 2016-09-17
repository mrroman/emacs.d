(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "it" "describe" "beforeEach" "afterEach" "before" "after"))

;; custom indent for package.json

(defun my/custom-js-indent ()
  (lexical-let* ((buffer-file-path (buffer-file-name (current-buffer)))
                 (fname (file-name-nondirectory buffer-file-path)))
                             (when (string= fname "package.json")
                               (setq-local js-indent-level 2))))

(defun my/eslint-custom-path ()
  "Try to use eshint from local project."
  (when (projectile-project-p)
    (lexical-let ((my/eslint-exec-path (projectile-expand-root "node_modules/.bin/eslint")))
      (when (file-exists-p my/eslint-exec-path)
        (setq-local flycheck-javascript-eslint-executable my/eslint-exec-path)))))

(add-hook 'js-mode-hook 'my/custom-js-indent)
(add-hook 'js-mode-hook 'my/eslint-custom-path)
