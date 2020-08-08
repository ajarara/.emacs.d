(defun my-find-init-file ()
  "Displays the contents of ~/.emacs.d/myinit.org, if already shown, revert to previous buffer"
  (interactive)
  (let ((init-file-location "/home/ajarara/.emacs.d/x250.org"))
    (if (string= init-file-location (buffer-file-name))
        (previous-buffer)
      (find-file init-file-location))))

(provide 'setup-personal-x250)
