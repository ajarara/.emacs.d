(require 'package)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; use-package automatic install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(defvar my-package-list
  `("naked.el"
    "my-packages.el"
    "my-functions.el"
    "my-hooks.el"
    "my-org.el"
    ))

(defvar my/config-loaded
  (directory-files "~/.emacs.d/config" nil "^[^\.]*.el$")
  "all files found in config that are not hidden and end in .el")
;; load all files in the above variable.
;; alternative: add my config to the load path, then require everything?
(dolist (config-file-name my/config-loaded)
 (load (concat "~/.emacs.d/config/" config-file-name)))
