(use-package projectile
  :straight t
  :demand t
  :bind*
  (("C-c c" . projectile-compile-project)
   ("C-c f" . projectile-find-file))
  :config
  (setq projectile-completion-system 'ivy))

(provide 'setup-projectile)
