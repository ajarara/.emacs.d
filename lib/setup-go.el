
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'direnv-mode)
  (add-to-list 'auto-mode-alist'("\\.go" . go-mode)))

(provide 'setup-go)
