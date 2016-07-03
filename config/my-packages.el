;; all packages besides use package are placed here, including package config.
;; prefer melpa-stable

;;(setq use-package-always-pin `melpa-stable)

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t)

(use-package helm
  :ensure t
  :config
  ;; literally the only thing I use helm and not ivy for. When ivy gets inline bindings, maybe I'll get rid of this for good. 
  (global-set-key (kbd "M-x") `helm-M-x))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t))

;; need to set this using bind key*
(use-package swiper
   :ensure t
   :config
   (global-set-key (kbd "C-s") `swiper))

(use-package ace-window
  :ensure t
  ;; the asterisk denotes never to rebind this.
  :bind* (("M-p" . ace-window))
  :config
  )

;; Weechat removed, too much of a pain to ensure always up, I'll just use erc.
(use-package slime
  :ensure t
  :config
  (slime-setup)
  (setq inferior-lisp-program "/usr/bin/sbcl"))


(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))


;; awesome. so now emacs is default state, and evil is reachable by just typing in escape. all insert mode bindings are overriden by emacs bindings.
;; so far the context switch is clear: if I can type text, I'm in emacs state, otherwise use j/k to navigate, V.. etc. great.
(use-package evil
  ;; evil-leader is run bfore evil, so that leader keys work in scratch and messages 
  :init
  (use-package evil-leader
    ;; disabled until i find use for it
    :disabled t
    :ensure t
    :config
    ;; is this the best way to load it in? 
    (load (concat my-config-dir "aux/my-evil-leader-bindings.el")))

  :ensure t
  
  :config
  (evil-mode t)
  ;; the below is used to have emacs be the default state, but allow me to drop in to evil if need be.
  ;; more config is available in the URL contained within the progn
  (progn
    (defalias 'evil-insert-state 'evil-emacs-state) ; http://stackoverflow.com/a/27794225/2932728
    (setq evil-default-state 'emacs)
  ;; https://bitbucket.org/bastibe/.emacs.d/src/12d08ec90a6445787b028fa8640844a67182e96d/init.el?at=master&fileviewer=file-view-default
    (define-key evil-emacs-state-map [escape] 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    )
  )
  

(use-package elpy
  ;; disabled until I know how to program
  :disabled t
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package hyde
  :disabled t
  :ensure t
  :config
  (setq hyde-home))


(use-package geiser
  :disabled t
  :ensure t)

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

(use-package try
    :ensure t)

;; THEME
(use-package monokai-theme
  :ensure t
  :config
  (load-theme `monokai t))
