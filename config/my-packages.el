;; all packages besides use package are placed here, including package config.

(use-package magit
  :ensure t)


(use-package projectile
  :ensure t)


(use-package helm
  :ensure t
  :config
  ; overrides default find file and executes
  (global-set-key (kbd "M-x") `helm-M-x)
  (use-package helm-mt
    ;; disabled until I figure out how to get C-d sending EOF working, and vim hooks
    :disabled
    :ensure t
    ))


(use-package weechat
  :ensure t)

(use-package slime
  :ensure t
  :config
  (slime-setup)
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package guide-key
  :ensure t)

(use-package evil
  ;; evil-leader is run before evil, so that leader keys work in scratch and messages 
  :init
  (use-package evil-leader
    :ensure t
    :config
    ;; is this the best way to load it in? 
    (load "~/.emacs.d/config/my-evil-leader-bindings.el"))

  :ensure t
  :config
  (evil-mode 1)
  ;; i know some of you may raise your eyebrows at this, but
  ;; this way i only have to disable one use-package declaration
  (use-package evil-org
    :ensure t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
