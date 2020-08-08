(add-hook 'apropos-mode-hook (lambda () (local-set-key (kbd "C-c f") 'apropos-follow)))

(provide 'setup-apropos)
