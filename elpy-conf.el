
(package-initialize)
(require 'subr)
(require 'elpy)
(let ((preference (y-or-n-p "Emulate custom elpy environment?")))
  (when preference
    (elpy-set-test-runner 'elpy-test-pytest-runner)
    (setq python-shell-completion-native-enable nil)
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python3")
    (setq python-shell-interpreter "python3")))
(elpy-enable)
