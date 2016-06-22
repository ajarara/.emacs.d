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
  ;; literally the only thing I use helm and not ivy for. When ivy gets inline bindings, maybe I'll get rid of this for good. 
  (global-set-key (kbd "M-x") `helm-M-x)
  )

(use-package swiper
   :ensure t
   :config
   (global-set-key (kbd "C-s") `swiper)
   (ivy-mode t)
   )
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") `ace-window)
  )

;; Weechat removed, too much of a pain to ensure always up, I'll just use erc.
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
  (global-set-key (kbd "C-z") `evil-mode)
  (use-package evil-leader
    :ensure t
    :config
    ;; is this the best way to load it in? 
    (load (concat my-config-dir "aux/my-evil-leader-bindings.el")))

  :ensure t
  :config
  ;; weaning off evil
  (evil-toggle-key "C-z")
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


(use-package geiser
  :ensure t)

(use-package try
  :ensure t)
;; THEME
(use-package monokai-theme
  :ensure t
  :config
  (load-theme `monokai t))
