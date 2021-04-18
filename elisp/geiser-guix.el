

;;;###autoload
(defvar geiser-guix--user-dir (file-name-directory load-file-name))
  
(defun geiser-guix--parameters ()
  (string-join
   (append '("--" geiser-guix--user-dir "shim.scm") (geiser-guile--parameters)) " "))

(defun geiser-guix--version (binary)
  (car (process-lines binary "repl" "--" "version.scm")))

(define-geiser-implementation (guix guile)
  (binary "guix")
  (version-command )
  (arglist geiser-guix--parameters))

(provide 'geiser-guix)
