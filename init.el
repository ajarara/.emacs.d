(require 'package)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; just so I have the flexibility of changing install directory later on
(setq default-directory "~/.emacs.d/")
(setq my-config-dir (concat default-directory "config/"))

;; use-package automatic install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use package config (shame I can't (use-package use-package))
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(defvar my/config-loaded
  (directory-files my-config-dir nil "^[^\.]*.el$")
  "all files found in config that are not hidden and end in .el.\n Warning: Currently thinks directories ending in .el are files")
;; load all files in the above variable.
;; alternative: add my config to the load path, then require everything?
(dolist (config-file-name my/config-loaded)
  (load (concat my-config-dir config-file-name)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
