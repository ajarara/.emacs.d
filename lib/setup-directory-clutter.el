(make-directory "~/.emacs.d/backup" t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

(make-directory "~/.emacs.d/autosave" t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(provide 'setup-directory-clutter)
