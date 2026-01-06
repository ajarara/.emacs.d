(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(magit-log-margin-show-committer-date t)
 '(native-comp-async-report-warnings-errors 'silent)
 '(safe-local-variable-values
   '((lisp-fill-paragraphs-as-doc-string nil)
     (eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (eval with-eval-after-load 'git-commit
           (add-to-list 'git-commit-trailers "Change-Id"))
     (visual-line-mode . t)
     (eval with-eval-after-load 'tempel
           (if (stringp tempel-path)
               (setq tempel-path (list tempel-path)))
           (let
               ((guix-tempel-snippets
                 (concat
                  (expand-file-name "etc/snippets/tempel"
                                    (locate-dominating-file
                                     default-directory
                                     ".dir-locals.el"))
                  "/*.eld")))
             (unless (member guix-tempel-snippets tempel-path)
               (add-to-list 'tempel-path guix-tempel-snippets))))
     (major-mode . shell-mode) (geiser-guile-binary "guix" "repl")
     (geiser-insert-actual-lambda) (geiser-repl-per-project-p . t)
     (eval add-to-list 'completion-ignored-extensions ".go")
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory
                                     ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (file-local-name
                    (expand-file-name root-dir-unexpanded)))
                  (root-dir* (directory-file-name root-dir)))
               (unless (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test
                           #'string-equal))))
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval progn (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph (&optional justify)
             (interactive "P")
             (or (fill-comment-paragraph justify)
                 (let
                     ((paragraph-start
                       (concat paragraph-start
                               "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                      (paragraph-separate
                       (concat paragraph-separate
                               "\\|\\s-*\".*[,\\.]$"))
                      (fill-column
                       (if
                           (and
                            (integerp emacs-lisp-docstring-fill-column)
                            (derived-mode-p 'emacs-lisp-mode))
                           emacs-lisp-docstring-fill-column
                         fill-column)))
                   (fill-paragraph justify))
                 t))
           (setq-local fill-paragraph-function
                       #'emacs27-lisp-fill-paragraph))
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file
                                    default-directory ".dir-locals.el"))))
             (unless (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (geiser-default-implementation quote guix)
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory
                                     ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir (expand-file-name root-dir-unexpanded))
                  (root-dir* (directory-file-name root-dir)))
               (unless (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test
                           #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
