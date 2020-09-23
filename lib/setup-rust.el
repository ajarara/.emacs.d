(use-package rust-mode
  :mode "\\.rs\\"
  :config
  (add-hook 'rust-mode-hook 'lsp))

(use-package cargo
  :hook rust-mode)


(provide 'setup-rust)

