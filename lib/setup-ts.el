(use-package tide
  :config
  (setq typescript-indent-level 2)
  (defun tide-setup-hanger ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (estq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)

    (tide-hl-identifier-mode 1)
    (local-set-key (kbd "M-?") 'tide-references)
    (company-mode 1))
  (add-hook 'typescript-mode-hook 'tide-setup-hanger))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))
  (setq typescript-indent-level 2))

(add-to-list 'projectile-project-root-files-bottom-up "package.json")
                   

(provide 'setup-ts)
