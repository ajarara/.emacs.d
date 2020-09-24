(use-package rust-mode
  :mode "\\.rs"
  :config
  (add-hook 'rust-mode-hook 'lsp))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'setup-rust)

