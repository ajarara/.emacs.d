;; It was a little difficult figuring out how to change tests
;; programmatically, so if you want to use something else, first M-x
;; elpy-set-test-runner , and then query elpy-test-runner. For
;; py.test, I had to use the symbol elpy-test-pytest-runner.

(use-package elpy
  :straight t
  :config

  ;; py.test is actively developed. 
  (elpy-set-test-runner `elpy-test-pytest-runner)

  ;; silences completion warning. found on ob-python's issue pages, strangely enough.
  (setq python-shell-completion-native-enable nil) 

  ;; preference
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  
  ;; start
  (elpy-enable))

(provide 'setup-elpy)
