;; All files depend on straight.el to manage packages, and use-package to configure
(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

;; Straight downloads a file to bootstrap from. Wondering why
;; bootstrap-version is defined in this block but never used. Even
;; though emacs has dynamic binding, I'm not setting the version
;; globally...

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; get load, unload goodness for setup files
(add-to-list 'load-path "~/.emacs.d/lib")

(straight-use-package 'use-package)

;; shouldn't be in here: We should just use general to write keybinds.
(require 'bind-key)
