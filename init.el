;; -------------------- PACKAGE INIT --------------------
(require 'package)
(package-initialize)

;; every time I see this I want to start using borg. it looks great.
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
;; TODO xml parser! there's a lib released on reddit that might help with this, it's new! GET HYPE search parsec
;; implemented: emacs state now has the same cursor as the replace key. I encountered a bug when setting emacs state cursor color, it reverted on normal mode. perhaps that's intentional, perhaps not. idk. (then again, probably intentional)
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

;; decent enough default theme, in case monokai is not available
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


;; registers are neat. let's try using them more often. If I bother creating a register, I want it to be persistent. Emacs calls persistent registers bookmarks, and to save them, you must either call `bookmark-save or configure it to save after x amount of bookmarks created
(setq bookmark-save-flag 1) ; so save after every bookmark made.

;; simple scrolling
(progn
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)
  )

;; -------------------- PACKAGES ---------------------

;; prefer melpa-stable

;;(setq use-package-always-pin `melpa-stable)
;; TODO list packages by order of use 

(use-package dired)

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


(use-package swiper
  :ensure t
  :config

  ;; almost required, I use search a lot for navigation, especially in this growing init file.
  (setq swiper-action-recenter t)
  ;; shadows isearch
  :bind* (("C-s" . swiper))
  )

(use-package ace-window
  :ensure t
  :bind*
  ;; shadows fill-paragraph
  (("M-q" . ace-window)
   ;; despite quoted-insert growing on me, maybe that's better reserved for something to be used in evil-leader, <leader> q or something, as that's definitely something I'll use in normal mode often.
   ;; shadows quoted-insert
   ("C-q" . ace-window))
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

(use-package evil
  ;; evil-leader is run before evil, so that leader keys work in scratch and messages 
  :init
  (setq evil-toggle-key "C-`")
  
  ;; evil's undo is a little strong, especially since I'm staying insert mode more often.
  (setq evil-want-fine-undo t)

  ;; since I have evil everywhere now, this might be neat. only works in normal mode, obviously.
  (use-package evil-leader
    :config
    (setq evil-leader/leader "<SPC>")
    
    (evil-leader/set-key "g" `keyboard-quit)

    (evil-leader/set-key "SPC" `ace-window)
    
    (evil-leader/set-key "w" `save-buffer)
    (evil-leader/set-key "v" `visual-line-mode)
    (evil-leader/set-key "t" `toggle-word-wrap)
    (evil-leader/set-key "s" `magit-status)
    
    (evil-leader/set-key "f" `find-file)
    (evil-leader/set-key "p" `projectile-find-file)
    
    (global-evil-leader-mode)
    )
  
  ;; don't actually use this at all, just couldn't set it to nothing
  :ensure t
  
  ;; notice the lack of the previous comment.
  :bind* (:map evil-emacs-state-map
	       ("C-r" . evil-paste-from-register)
	       :map evil-normal-state-map
	       ("j" . evil-next-visual-line)
	       ("k" . evil-previous-visual-line)
	       ("'" . evil-goto-mark)
	       ("C-y" . yank))
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
  (setq evil-emacs-state-cursor `(hbar . 2))
  

  
  (use-package evil-visual-mark-mode
    :ensure t
    :config
    (evil-visual-mark-mode))
  )

(use-package term
  :config
  ;; all of this config is from:
  ;; http://echosa.github.io/blog/2012/06/06/improving-ansi-term/

  ;; kill the buffer after finishing.
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
	(let ((buffer (process-buffer proc)))
	  ad-do-it
	  (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  ;; don't ask me about whether I want to use bash. I do.
  ;; modified from ansi-term to term from source post
  (defvar my-term-shell "/bin/bash")
  (defadvice term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'term)
  
  ;; why is this not the default?
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)
 
  (add-hook 'term-mode-hook 'goto-address-mode)

    
  
  :bind*
  (("C-z" . term))
)



(use-package expand-region
  :ensure t
  :bind (("M-t" . er/expand-region))
  )


(use-package sml-mode
  :ensure t
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
  (bind-key "C-h SPC" `which-key-show-top-level)
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

;; I don't know if this is the best idea, seeing as how I ALWAYS use ace-window, but it makes a little sense to do this with the evil windowing system.

;; shadows help
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

;; shadows prefix containing occur
(bind-key* "M-s" `switch-to-buffer)

;; shadows tab-to-tab-stop
(bind-key* "M-i" `my/find-init-file)

;; instantly kills buffer (without deleting the window), unless unsaved content. this advices kill-buffer
;; shadows kill-sentence
(bind-key* "M-k" `kill-this-buffer)

;; U for undeaaaaaaaaaaaaaaaaad
;; shadows upcase-word
(bind-key* "M-u" `bury-buffer)

;; shadows nothing that I know of.
(bind-key* "M-p" `my/find-projects)


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

    (my/prefix-add-to-map my-map "o" `my/find-org-files)
    (my/prefix-add-to-map my-map "r" `org-capture)
    (my/prefix-add-to-map my-map "M-r" `org-capture)
    
    
    ;; oh emacs, some people think you don't make any sense
    ;; but I'll just chalk it up to charm.
    (my/prefix-add-to-map my-map "v" `split-window-horizontally)
    (my/prefix-add-to-map my-map "M-v" `split-window-horizontally)
    (my/prefix-add-to-map my-map "h" `split-window-vertically)
    (my/prefix-add-to-map my-map "M-h" `split-window-vertically) 
    
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

(defun my/find-org-files ()
  "navigates to ~/Documents/org"
  (interactive)
  (ido-find-file-in-dir "~/Documents/org/"))

;; -------------------- HOOKS --------------------

;; org mode hooks
(add-hook `org-mode-hook `org-indent-mode)
(add-hook `org-mode-hook `visual-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (counsel-projectile counsel sml-mode expand-region))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
