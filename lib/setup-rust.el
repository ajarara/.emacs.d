(use-package rust-mode
  :mode "\\.rs\\"
  :hook lsp)

(use-package cargo
  :hook rust-mode)


(provide 'setup-rust)

