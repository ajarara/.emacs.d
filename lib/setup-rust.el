;; Oh, you knew it was coming.
(straight-use-package 'rust-mode)
  (use-package rust-mode)
(straight-use-package 'cargo)
  (use-package cargo)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)

  ;; somewhere I read that using add-to-list is not enough, even though it mutates the list..
  ;; I'll leave out the setq at first and see if emacs cooperates 
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

(provide 'setup-rust)

