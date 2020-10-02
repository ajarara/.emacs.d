
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp)
  (add-to-list 'auto-mode-alist'("\\.go" . go-mode)))
