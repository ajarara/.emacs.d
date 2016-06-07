(defun my-open-file ()
  "Open file using projectile+Helm or ido"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))

;; should I bind keys to functions in the same file I define them? hm..
;; let's try it out.

(global-set-key (kbd "M-o") `my-open-file)
