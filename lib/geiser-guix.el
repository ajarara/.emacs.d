
(defun geiser-guix--parameters ()
  (string-join
   (append '("--" "shim.scm") (geiser-guile--parameters)) " "))

(define-geiser-implementation (guix guile)
  (binary "guix")
  (arglist geiser-guix--parameters))

(provide 'geiser-guix)
