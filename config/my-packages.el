;; all packages besides use package are placed here, including package config.
;; prefer melpa-stable

;;(setq use-package-always-pin melpa-stable)
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
    )
  (use-package helm-projectile
    ;; disabled until I figure out how to pin melpa-stable as default
    :disabled t
    :ensure t)
  )


;; weechat removed, too much of a pain to ensure always up, I'll just use erc.
(use-package slime
  :ensure t
  :config
  (slime-setup)
  (setq inferior-lisp-program "/usr/bin/sbcl"))



(use-package guide-key
  ;; which key is better
  :disabled t
  :ensure t)

(use-package evil
  ;; evil-leader is run before evil, so that leader keys work in scratch and messages 
  :init
  (use-package evil-leader
    :ensure t
    :config
    ;; is this the best way to load it in? 
    (load (concat my-config-dir "aux/my-evil-leader-bindings.el")))

  :ensure t
  :config
  ;; need a bind to toggle evil mode
  (evil-mode 1)
  ;; i know some of you may raise your eyebrows at this, but
  ;; this way i only have to disable one use-package declaration
  ;; magit is awesome, evil is awesome, leader keys are awesome
  (use-package evil-magit
    ;; reenabled evil-magit. I need vim keys for going through diffs.
    :ensure t)
  (use-package evil-org
    ;; not maintained anymore it seems, further conflicts with leader bindings (seriously, don't mess with my safe space)
    :disabled t
    :ensure t))

(use-package elpy
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package hyde
  :ensure t
  :config
  (setq hyde-home))
;; THEME
(use-package monokai-theme
  :ensure t
  :config
  (load-theme `monokai t))

(use-package geiser
  :ensure t)
