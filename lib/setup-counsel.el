(use-package counsel
  :straight t
  :bind* (("M-x" . counsel-M-x)
          ("C-c a" . counsel-ag)))

(provide 'setup-counsel)
