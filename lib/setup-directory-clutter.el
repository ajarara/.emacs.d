(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(provide 'setup-directory-clutter)
