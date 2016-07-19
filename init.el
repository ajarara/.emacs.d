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

;; yes I know I should use org mode for this..
;; TODO move all my stuff into a big init file
;; TODO not emacs related, make keyboard config a single shell script
;; TODO emacs related, find out the most used org capture template keywords
;; TODO xml parser! there's a lib released on reddit that might help with this, it's new! GET HYPE
;; . as super modifier? Only problem is ". " How about /, but that's too much. Q is also considered. need ideas. semicolon works GREAT
;; maybe it's a good idea to split shift usage. there was a guy who wrote something that ignored all input unless you used the right keys.
;; here's what I wanted to write initially: make the cursor color change based on modes. Yes I can look down but that's too slow. maybe some red or something, use the monokai theme as a guide. the defun color?

;; -------------------- NAKED-CONFIG --------------------
;; configuration that doesn't rely on any external packages, with the exception of bind-key, because it's very useful.


;; this is needed, otherwise emacsclient hangs upon exit when it has something in the kill ring
(setq x-select-enable-clipboard-manager nil)

;; removing all the visual goodies
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; add in a useful visual goodie
(visual-line-mode 1)


;; why is this the only way to set word wrap... -_-
(toggle-word-wrap)

;; decent enough theme, in case monokai is not available
;;(load-theme 'misterioso t)

;; another example of stupid variable interfaces. if I'm setting this, I'd want to have debugging on all the time.
;; but I have to unset this when I explicitly enable init-debugging or something.
;;(toggle-debug-on-error)


;; FROM http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
;; store all backup and autosave files in the tmp dir
;; still have to deal with garbage files, though. it's really god damn annoying to have those be added in git
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; is there a way in vim to insert just one character at a specific place without having to go in insert mode?
(setq split-height-threshold nil)
;; tried 150, I'm using xfwm4 if that makes any difference, but it did not work.
(setq split-width-threshold 140)

;; please enter yes/no, no screw that, I just want y/n
(fset `yes-or-no-p `y-or-n-p)

;; echo keystrokes quicker, helps a lot with prefix keys
;; heavily recommend which-key to the emacs newbie
(setq echo-keystrokes 0.1)

;; paste at point, not on location of click (thanks wasamasa!)
;; his config is here: https://github.com/wasamasa/dotemacs/blob/master/init.org
(setq mouse-yank-at-point t)

;; why dialog boxes are a thing are beyond me
(setq use-dialog-box nil)


;; rebinding the weird stuff. the asterisk designates: don't override this, ever
;; M-i shadows tab-to-tab-stop, it inserts tabs and I'm firmly a spaces guy. If I want to indent something, I'll use C-M-\, or do it by hand.
;; imenu is alright, tbh. idk why I bound it here.
(bind-key* "M-i" `imenu)

;; needed to get around the whole C-i == tab debacle. don't have a Hyper key, or plan on configuring one, so this works decent enough.
;; does not work, unfortunately. <tab> still yields init-file. need to replace it with something else.
;;(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
;;(bind-key* "C-i" `my/find-init-file)


;; no reason to have C-i bound to Tab when I have Tab right there.
;; warning, this does not work if you run in a terminal. C-i and C-m are firmly linked to tab and return.

;; top level keys I don't mind rebinding, in order of uselessness
;; C-m C-q C-j M-u 


;; -------------------- PACKAGES ---------------------

;; prefer melpa-stable

;;(setq use-package-always-pin `melpa-stable)
;; TODO list packages by order of use 

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


(use-package yasnippet
  :ensure t
  :config
  (use-package common-lisp-snippets
    :ensure t)
  (yas-global-mode))

(use-package swiper
   :ensure t
   :config
   (global-set-key (kbd "C-s") `swiper))


(use-package ace-window
  :ensure t
  ;; the asterisk denotes never to rebind this.
  ;; why is the binding outside this block?
  :config
  )

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


(use-package evil
  ;; need to disable until I actually read the damn manual, then I will reenable
  ;; evil-leader is run before evil, so that leader keys work in scratch and messages 
  :init
  ;; don't actually use this at all, just couldn't set it to nothing
  (setq evil-toggle-key "C-`")
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
  
  ;; IDK about motion state, it blocks useful keys.
  ;; that said, this isn't perfect. there are some things I'd like to modify.

  ;; I'd like C-y to 'yank' even while in normal mode.
  (define-key evil-normal-state-map (kbd "C-y") 'yank)
  
  ;; todo alias 0 to C-a
  ;; todo change cursor color based on emacs/normal state

  ;; I like the use of registers (while in insert mode), and I don't really need isearch backwards when I've got swiper which is a whole lot better
  ;; notice I modify evil-emacs state, as I've aliased it to insert state.
  (define-key evil-emacs-state-map (kbd "C-r") 'evil-paste-from-register)
  
  ;; marks are nice. would be really nice if there was a way to do global marks.
  (define-key evil-normal-state-map (kbd "'") `evil-goto-mark)
  
  ;; since I have evil everywhere now, this might be neat. only works in normal mode, obviously.
  (use-package evil-leader
    :config
    (evil-leader/set-leader ",")
    )
  
  (use-package evil-visual-mark-mode
    :ensure t
    :config
    (evil-visual-mark-mode))
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

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))


;;
(use-package try
    :ensure t)


;; THEME
(use-package monokai-theme
  :ensure t
  :config
  (load-theme `monokai t))


;; init or config? I never know.
(use-package org
  :init
  (setq org-directory "~/Documents/org/")

  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (define-key global-map (kbd "<f5>") `org-capture)


  ;; capture templates that work, as of now.
  ;; for more info, check out http://orgmode.org/manual/Capture-templates.html
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/gtd.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ("e" "Emacs" entry (file+datetree "~/Documents/org/emacs.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
          ("k" "KOL" entry (file+datetree "~/Documents/org/kol.org")
	   "* %?\nEntered on %U\n %a")
	  ("m" "track" entry (file+datetree "~/Documents/org/track.org")
	   "* %?\nEntered on %U\n")
	  ("g" "grievances" entry (file+datetree "~/Documents/org/grievances.org")
	   "* %?\nEntered on %U\n")
	  )
	)
  )


;; should I move this func to the my namespace?
;; meh.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; I type it enough for it to be useful.
;; maybe a cool package idea would be to suggest a more interactive way to define keybinds
;; based on frequency of M-x invocations
;; another would be a visualization of all keybinds of all minor/major modes installed, so you
;; can find a key that you'll be fine with 
(global-set-key (kbd "C-h SPC") `which-key-show-top-level)

(defun my/znc ()
  (interactive)
  (erc-tls
   :server "jarmac.org"
   :port 6697
   ))

;; planning on moving all this functionality to bind-key.

(defvar my-prefix "C-j ")
(defun my/prefix-add-to-map (map key-as-string function-symbol)
  (define-key map (kbd (concat my-prefix key-as-string)) function-symbol))

(defvar my/mode-map
  (let ((my-map (make-keymap)))
    
    ;; top level binds
    (define-key my-map (kbd "C-q") `ace-window)
    ;;(define-key my-map (kbd "C-z") `eshell)
;;    (define-key my-map (kbd "M-h") `switch-to-buffer)
    (define-key my-map (kbd "M-r") `delete-other-windows)
    (define-key my-map (kbd "M-w") `delete-window)
    (define-key my-map (kbd "M-q") `kill-buffer)
    ;; this doesn't work well, it gets rebound to tab. need another keybind. C-m doesn't work either, for the same stupid reason.
    ;;(define-key my-map (kbd "C-i") `my/kill-other-window)
    
    ;; muddling in other keymaps
    (define-key my-map (kbd "C-h SPC") `which-key-show-top-level)
    
    ;; under my/prefix
    (my/prefix-add-to-map my-map "V" `visual-line-mode)
    (my/prefix-add-to-map my-map "v" `split-window-horizontally)
    (my/prefix-add-to-map my-map "h" `split-window-vertically) ; yeah, it doesnt make sense.

    (my/prefix-add-to-map my-map "C-g" `keyboard-quit) 
    (my/prefix-add-to-map my-map "C-j" `ace-window)
    (my/prefix-add-to-map my-map "C-0" `text-scale-adjust)
    (my/prefix-add-to-map my-map "C-r" `org-capture)
    (my/prefix-add-to-map my-map "C-q" `quoted-insert)
    (my/prefix-add-to-map my-map "C-f" `ffap)
    (my/prefix-add-to-map my-map "C-s" `magit-status)
    
    ;; under my/prefix with a custom func
    (my/prefix-add-to-map my-map "C-t" `my/toggle-window-split)
    (my/prefix-add-to-map my-map "C-c" `my/find-emacs-config-file)
    (my/prefix-add-to-map my-map "C-p" `my/find-projects)
    my-map
    ))

;; all homemade functions can be found under this minor mode declaration
(define-minor-mode my/mode
  :diminish
  :global
  :keymap `my/mode-map
  )
;; evaluate it. considering moving to johnw's bind-key so that I can declare these keybinds in use-package configs
(my/mode)

	
(defun my/kill-other-window ()
  (interactive)
  (if (= (count-windows) 2)
      (progn
	(other-window 1)
	(kill-buffer))
    (error "This only works when there are two buffers!")))

		  
;; not mine, found off of emacs-wiki. quickly switches orientation of two buffers.
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; quick way to find configuration files
(defun my/find-emacs-config-file ()
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/config/"))

(defun my/find-init-file ()
  "Displays the contents of ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; quick way to nav projects
(defun my/find-projects ()
  (interactive)
  (ido-find-file-in-dir "~/Documents/projects/"))

;; Next stop. Gift shop.

;; org mode hooks
(add-hook `org-mode-hook `org-indent-mode)
(add-hook `org-mode-hook `visual-line-mode)
