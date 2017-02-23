
(package-initialize)
(require 'elpy)
(let ((preference (y-or-n-p "Emulate custom elpy environment?")))
  (when preference
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python3")))
(elpy-enable)
