(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "it" "describe" "beforeEach" "afterEach" "before" "after"))

;; custom indent for package.json

(defun my/custom-js-indent ()
  (lexical-let* ((buffer-file-path (buffer-file-name (current-buffer)))
                 (fname (file-name-nondirectory buffer-file-path)))
                             (when (string= fname "package.json")
                               (setq-local js-indent-level 2))))

(defun my/npm-dep-bin-path (name)
  "Returns path to npm deps binary."
  (projectile-expand-root (concat "node_modules/.bin/" name)))

(defun my/js-linters-setup ()
  "Looks for jslinters in project devdependencies and use them if declared."
  (when (projectile-project-p)
    (lexical-let ((package-json-file (projectile-expand-root "package.json")))
      (when (file-exists-p package-json-file)
        (lexical-let* ((package-json (json-read-file package-json-file))
                       (dev-deps (alist-get 'devDependencies package-json)))
          (cond ((alist-get 'eslint dev-deps)
                 (setq-local flycheck-javascript-eslint-executable (my/npm-dep-bin-path "eslint")))
                ((alist-get 'jshint dev-deps)
                 (setq-local flycheck-javascript-jshint-executable (my/npm-dep-bin-path "jshint")))))))))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(add-hook 'js-mode-hook 'my/custom-js-indent)
(add-hook 'js-mode-hook 'my/js-linters-setup)
