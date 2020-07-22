;; language specific config (rust, typescript, go) should be in their
;; own setup files so that they are never in use on specific machines.

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-enable-which-key-integration)))
