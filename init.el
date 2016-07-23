;; -------------------- PACKAGE INIT --------------------
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

;; use package config (shame I can't (use-package use-package))
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))
;; a utility that removes a minor mode from the modeline
(require 'diminish)
(require 'bind-key)

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; yes I know I should use org mode for this..
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




;; -------------------- PACKAGES ---------------------

;; prefer melpa-stable

;;(setq use-package-always-pin `melpa-stable)
;; TODO list packages by order of use 

(use-package helm
  :init
  ;; helm sets this stuff off, and they're not gonna fix it: https://github.com/emacs-helm/helm/issues/1498#issue-154021209
  (setq ad-redefinition-action 'accept) 
  :ensure t
  :bind* (("M-x" . helm-M-x)))

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
  :bind* (("C-s" . swiper))
  )

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

(use-package circe
  :ensure t
  :config
  (setq circe-reduce-lurker-spam t)
  (setq circe-network-options
	'(("ZNC"
	   :tls t
	   :host "jarmac.org"
	   :port 6697
	   :nick "alphor"
	   ))))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package evil
  ;; evil-leader is run before evil, so that leader keys work in scratch and messages 
  :init
  (setq evil-toggle-key "C-`")
  ;; since I have evil everywhere now, this might be neat. only works in normal mode, obviously.

  (use-package evil-leader
    :config
    (setq evil-leader/leader "<SPC>")
    
    (evil-leader/set-key "g" `keyboard-quit)
    
    (evil-leader/set-key "w" `save-buffer)
    (evil-leader/set-key "v" `visual-line-mode)
    (evil-leader/set-key "t" `toggle-word-wrap)
    (evil-leader/set-key "s" `magit-status)
    (global-evil-leader-mode)
    )
  
  ;; don't actually use this at all, just couldn't set it to nothing
  :ensure t
  
  ;; notice the lack of bind*. since I'm binding to a map, I don't want this to be global.
  :bind* (:map evil-emacs-state-map
	       ("C-y" . yank)
	       ("C-r" . evil-paste-from-register)
	       :map evil-normal-state-map
	       ("j" . evil-next-visual-line)
	       ("k" . evil-previous-visual-line))

  :bind-keymap*
  (("C-w" . evil-window-map))
  
  ;; the bind keyword lazy loads evil until you use one of the binds. I don't wanna do that, instead, I want it to load immediately.
  :demand
  :config
  (evil-mode t)
  ;; the below is used to have emacs be the default state, but allow me to drop in to evil if need be.
  ;; more config is available in the URL contained within the progn
  (progn
    (defalias 'evil-insert-state 'evil-emacs-state) ; http://stackoverflow.com/a/27794225/2932728
    (setq evil-default-state 'emacs)
    ;; https://bitbucket.org/bastibe/.emacs.d/src/12d08ec90a6445787b028fa8640844a67182e96d/init.el?at=master&fileviewer=file-view-default
    (define-key evil-emacs-state-map [escape] 'evil-normal-state)
    )
  ;; I didn't put the above define-key into the bind just because it makes more sense here. If I encounter a remapping of esc, I'd probably move it into bind*
  
  ;; IDK about motion state, it blocks useful keys, like ? or h.

  ;; a quick way to differentiate which state I'm in without looking at the mode line, may change this later.
  (setq evil-emacs-state-cursor `bar)

  
  (use-package evil-visual-mark-mode
    :ensure t
    :config
    (evil-visual-mark-mode))
  )

;; has to be after evil has been loaded in. For some reason, evil does not want to let go of the C-z key.
;; better than ansi-term. has sane defaults, allows me to M-x, C-h still operates, multi-term-next spawns a new shell if there aren't any, and cycles!
(use-package multi-term
  :ensure t
  :bind* (("C-z" . multi-term-next))
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


(use-package try
  :ensure t
  )


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
          ("a" "ascension" entry (file+datetree "~/Documents/org/kol-ascension.org")
	   "* %?\nEntered on %U\n %a")
	  ("m" "track" entry (file+datetree "~/Documents/org/track.org")
	   "* %?\nEntered on %U\n")
	  ("g" "grievances" entry (file+datetree "~/Documents/org/grievances.org")
	   "* %?\nEntered on %U\n")
	  )
	)
  :bind*
  (("<f5>" . org-capture))
  )

;; -------------------- TOP LEVEL BINDS --------------------
;; uh... NOT TIED TO A SPECIFIC PACKAGE

;; M-x describe-personal-keybinds
;; interestingly enough, this doesn't apply if you override-global-map. is that all the asterisk does? (override-global-map, not the small effect on desc-personal)
;; list of top level binds I don't use, ever. (aka mostly highlight keys that I'd like to be encapsulated in visual mode)
;; C-q M-q M-e M-y M-i M-o C-i (tab)
;; M-c (upcase word? nty)
;; M-u? maybe. M-k
;; shadows means stock emacs ships with this bound, but I rebind it.

;; shadows fill-paragraph
(bind-key* "M-q" `ace-window)
;; I don't know if this is the best idea, seeing as how I ALWAYS use ace-window, but it makes a little sense to do this with the evil windowing system.
;; despite quoted-insert growing on me, maybe that's better reserved for something to be used in evil-leader, <leader> q or something, as that's definitely something I'll use in normal mode often.
;; shadows quoted-insert
(bind-key* "C-q" `ace-window)

;; shadows universal arg, I think? Damn, I need to read the manual.
(bind-key* "C-0" `text-scale-adjust)

;; shadows move-to-window-line-top-bottom 
(bind-key* "M-r" `delete-other-windows)

;; would like this instead to just kill the buffer, or like rotate. I think I need some buffer management tool
;; shadows kill-ring-save
(bind-key* "M-w" `delete-window)

;; I don't actually know what the name of the function is, but I know I don't need it. It's some typeface stuff.
;; also, the function name here is misleading, it evaluates the whole top-level expression, from anywhere in the expression.
;; shadows Set face:
(bind-key* "M-o" `eval-defun)

;; I'm gonna need shackle just for this async.
;; shadows universal argument, 7
(bind-key* "M-7" `async-shell-command)

;; shadows universal argument, 1
(bind-key* "M-1" `shell-command)


;; shadows tab-to-tab-stop
(bind-key* "M-i" `my/find-init-file)
;; shadows nothing that I know of.
(bind-key "M-p" `my/find-projects)

;; I type it just enough for it to be useful.
;; maybe a cool package idea would be to suggest a more interactive way to define keybinds
;; based on frequency of M-x invocations
;; another would be a visualization of all keybinds of all minor/major modes installed, so you
;; can find a key that you'll be fine with 
;; shadows nothing that I know of, hence the lack of *
(bind-key "C-h SPC" `which-key-show-top-level)



;; -------------------- CUSTOM-FUNCTIONS ---------------------

;; something useful from the emacs wiki? No way.
(defun my/smarter-move-beginning-of-line (arg)
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
                'my/smarter-move-beginning-of-line)



;; -------------------- MY-MODE (to be pruned) ---------------------

;; planning on moving all this functionality to bind-key. bind-keymap.
;; got rid of this because I'd like to use top level binds for most of these extremely common operations. unfortunately, there is no easy way to unbind these temporarily without commenting the relevant lines out, then reloading the entirety of emacs. if there was a package that rebound stock keys or dynamically rebound them to something, that would be neat!
;; oh jeeze, emacs thought of it already: describe-personal-keybindings
;; okay emacs, how about a package that looks at all the keymaps installed (but not necessarily active) and checking if they redefine a specific key? No? Hah.

(defvar my-prefix "M-c ")
(defun my/prefix-add-to-map (map key-as-string function-symbol)
  (define-key map (kbd (concat my-prefix key-as-string)) function-symbol))

(defvar my/mode-map
  (let ((my-map (make-keymap)))
    
    ;; this doesn't work well, it gets rebound to tab. need another keybind. C-m doesn't work either, for the same stupid reason.
    ;;(define-key my-map (kbd "C-i") `my/kill-other-window)
    
    ;; under my/prefix
    (my/prefix-add-to-map my-map "s" `magit-status)
    (my/prefix-add-to-map my-map "M-s" `magit-status)

    (my/prefix-add-to-map my-map "z" `evil-emacs-state)
    (my/prefix-add-to-map my-map "M-z" `evil-emacs-state)
    
    (my/prefix-add-to-map my-map "g" `keyboard-quit)
    (my/prefix-add-to-map my-map "M-g" `keyboard-quit)
    (my/prefix-add-to-map my-map "C-g" `keyboard-quit) ; hell, why not

    (my/prefix-add-to-map my-map "r" `org-capture)
    (my/prefix-add-to-map my-map "M-r" `org-capture)
    
    
    ;; under my/prefix with a custom func
    (my/prefix-add-to-map my-map "p" `my/find-projects) ; adding a meta prefix won't make much sense here, based on key layout

    (my/prefix-add-to-map my-map "t" `my/toggle-window-split)
    (my/prefix-add-to-map my-map "M-t" `my/toggle-window-split)
    
    ;; return my-map
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

;; -------------------- FUNCTIONS --------------------
	
(defun my/kill-other-window ()
  (interactive)
  (if (= (count-windows) 2)
      (progn
	(other-window 1)
	(kill-buffer)
	(other-window 1))
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


(defun my/find-init-file ()
  "Displays the contents of ~/.emacs.d/init.el, if already shown, revert to previous buffer"
  (interactive)
  (let ((init-file-location "/home/ajarara/.emacs.d/init.el"))
    (if (string= init-file-location (buffer-file-name))
	(previous-buffer)
      (find-file init-file-location)))
  )

  (buffer-file-name)

(defun my/find-projects ()
  "navigates to ~/Documents/projects"
  (interactive)
  (ido-find-file-in-dir "~/Documents/projects/"))

;; -------------------- HOOKS --------------------

;; org mode hooks
(add-hook `org-mode-hook `org-indent-mode)
(add-hook `org-mode-hook `visual-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (epkg))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
