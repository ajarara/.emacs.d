
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-protocol 'ssh)

(straight-use-package
 '(use-package
    :type git :host github :repo "alphor/use-package"
    :upstream (:host github :repo "jwiegley/use-package")))

;; bind-key is provided with use-package, diminish I only use once or twice
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; this didn't seem to work, would not have the first frame be terminus'd
;; (set-face-attribute 'default nil :font "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
;; this worked!
(add-to-list 'default-frame-alist '(font . "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1"))

;; can be recovered with <F10>
(tool-bar-mode -1)

;; not needed, just look at the mode line for gauging where you are in the buffer
(scroll-bar-mode -1)

;; wait which one is which
(menu-bar-mode -1)

;; you can set this to some arbitrary number so that it will blink for that number of times. I don't like blinking.
(blink-cursor-mode 0)

;; pretty quotes can't be jumped to easily.
(setq text-quoting-style 'grave)

;; enabling visual line mode
(visual-line-mode 1)

(toggle-word-wrap)

(defvar my-hl-line-mode-hook-list
  `(prog-mode-hook
    circe-mode-hook))

(dolist (this-mode-hook my-hl-line-mode-hook-list)
  (add-hook this-mode-hook `hl-line-mode))

(setq frame-title-format (concat "%b" " " invocation-name "@" (system-name)))

(fset `yes-or-no-p `y-or-n-p)

(setq echo-keystrokes 0.1)

(setq mouse-yank-at-point t)

(setq vc-follow-symlinks nil)

(setq disabled-command-function nil)

(use-package lsp-mode
  :ensure t
    :recipe (lsp-mode :type git :host github :repo "alphor/lsp-mode"
                      :upstream (:host github :repo "emacs-lsp/lsp-mode")))

(straight-use-package 'buttercup)

(straight-use-package 'evil)  ;; no plans to contrib upstream for now.
(use-package evil

:init
 (setq evil-toggle-key "C-`")

(setq evil-want-fine-undo t)

:config
(evil-mode t)

(progn
  (defalias 'evil-insert-state 'evil-emacs-state) ; http://stackoverflow.com/a/27794225/2932728
  (setq evil-default-state 'emacs)
  ;; https://bitbucket.org/bastibe/.emacs.d/src/12d08ec90a6445787b028fa8640844a67182e96d/init.el?at=master&fileviewer=file-view-default
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  )
;; I didn't put the above define-key into the bind just because it makes more sense here. If I encounter a remapping of esc, I'd probably move it into bind*

;; IDK about motion state, it blocks useful keys, like ? or h. (which I get to by typing "\" in normal mode)

(setq evil-emacs-state-cursor `(hbar . 2))

(setq evil-lookup-func (lambda () (call-interactively 'man)))

(straight-use-package 'evil-visual-mark-mode)
(use-package evil-visual-mark-mode
  :config
  (evil-visual-mark-mode))

:demand t

:bind* (:map evil-emacs-state-map
               ("C-r" . evil-paste-from-register)

               :map evil-normal-state-map
               ("C-f" . evil-scroll-down)
               ("C-b" . evil-scroll-up)
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("'" . evil-goto-mark)
               ("C-e" . end-of-line)
               ("C-y" . yank)
               ("C-d" . evil-scroll-down)
               ("C-t" . ace-window)

               :map evil-motion-state-map
               ("C-f" . evil-scroll-down)
               ("C-b" . evil-scroll-up))
) ;; closes use-package evil block

(straight-use-package 'general)
  (use-package general
    ;; maybe in the future make this config evil agnostic?
    :if (featurep 'evil)
    :config

    ;; leader key binds
    (setq general-default-keymaps '(evil-normal-state-map
                                    evil-visual-state-map))

    (setq general-default-prefix "SPC")
    (general-define-key
                        
     "g" 'keyboard-quit
     "C-g" 'keyboard-quit
     "SPC" 'ace-window

     "w" 'save-buffer
     "v" 'visual-line-mode
     "t" 'toggle-word-wrap
     "s" 'magit-status
     
     "a" 'org-agenda-list
     
     "m" 'fill-region

     ;; in the case that we don't have projectile, fall back to
     ;; vanilla find-file
     "f" (if (featurep 'projectile)
             'projectile-find-file
           'find-file)
     "p" 'my-find-projects
     "o" 'my-find-org-files

     "r" 'org-capture

     "i" 'imenu

     ) ;; closes general-define-key block
    
  ) ;; closes use-package general block

(use-package dired
  :config
  (define-key dired-mode-map (kbd "SPC") nil)
  (define-key dired-mode-map (kbd "M-s") nil)
  
  ;; remove dired-mode-map definition
  (define-key dired-mode-map (kbd "i") nil)
  
  (general-define-key :prefix nil
                      :keymaps 'dired-mode-map
                      :states '(normal)
                      "i" 'evil-insert-state)
                      
  (general-define-key :prefix nil
                      :keymaps 'dired-mode-map
                      :states '(emacs)
                      "i" 'dired-maybe-insert-subdir)

)

(setq tramp-default-method "ssh")

(straight-use-package 'ivy)
  (use-package ivy
    ;; :recipe (ivy :type git :host github :repo "abo-abo/swiper")
    :demand t
    :diminish ivy-mode
    :config
    (setq ivy-ignore-buffers `("\\` "))
    
    ;; i like completion in the minibuffer, completion in region is obnoxious when you have hl-line-mode active. This must be set before ivy-mode is called.
    (setcdr (assoc 'ivy-completion-in-region ivy-display-functions-alist) nil)

    (ivy-mode t))

  (use-package swiper
    :config

    ;; almost required, I use search a lot for navigation, especially in
    ;;   this growing init file. Note that if multiple candidates are in a
    ;;   view moving between them does not recenter the buffer.
    (setq swiper-action-recenter t)

    ;; shadows isearch
    :bind* (("C-s" . swiper))
    )

(straight-use-package 'counsel)
(use-package counsel
  :bind* (("M-x" . counsel-M-x)))

(straight-use-package 'ace-window)
  (use-package ace-window
    :bind*
    (("C-t" . ace-window))
    :config
    (setq aw-scope 'frame)
    )

(straight-use-package 'magit)
(use-package magit)

(use-package org
  :init
  (setq org-directory "~/Documents/org/")

  (setq org-default-notes-file (concat org-directory "notes.org"))

(setq my-org-capture-directory "~/Documents/org/capture/")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/gtd-capture.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("e" "Emacs" entry (file+datetree "~/Documents/org/emacs.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("k" "KOL" entry (file+datetree "~/Documents/org/kol.org")
         "* %?\nEntered on %U\n %a")
        ("a" "ascension" entry (file+datetree "~/Documents/org/kol-ascension.org")
         "* %?\nEntered on %U\n %a")
        ("r" "track" entry (file+datetree "~/Documents/org/track.org")
         "* %?\nEntered on %U\n")
        ("d" "dose" entry (file+datetree "~/Documents/org/dose.org")
         "* %?\nEntered on %U\n")
        ("g" "grievances" entry (file+datetree "~/Documents/org/grievances.org")
         "* %?\nEntered on %U\n %i")
        ("p" "programming" entry (file+datetree "~/Documents/org/programming.org")
         "* %?\nEntered on %U\n  %i")
        ("l" "laptop" entry (file+datetree "~/Documents/org/laptop.org")
         "* %?\nEntered on %U\n %i")
        ("m" "music" entry (file+datetree "~/Documents/org/music.org")
         "* %?\nEntered on %U\n %i")
        ("u" "uncategorized-mess" entry (file+datetree "~/Documents/org/u-mess.org")
         "* %?\nEntered on %U\n")
        ("h" "recurse" entry (file+datetree "~/Documents/org/recurse.org")
         "* %?\nEntered on %U\n")
        ("c" "coffee" entry (file+datetree "~/Documents/org/coffee.org")
         "* %?\nEntered on %U\n")
        )
      )

(setq org-agenda-files (list "~/Documents/org/gtd-capture.org"))

:bind*
(("<f6>" . org-capture))
)

(use-package term 
  :config
  ;; most of this config is from:
  ;; http://echosa.github.io/blog/2012/06/06/improving-ansi-term/

  ;; don't modify my output please (note this breaks when displaying
  ;; multiline commands at the bottom of the buffer)
  (setq term-suppress-hard-newline t)

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
  ;; in NixOS the shell is in /run/current-system
  ;; rather than dispatch on what OS I'm running, just let
  ;; which handle it:
  (defvar my-term-shell (s-trim (shell-command-to-string "which bash")))
  (defadvice term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'term)

  ;; why is this not the default? 
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)


  ;; eh.. this makes me sad. All I wanted was C-x.
  ;; (defun my-ad-term-line-mode (_arg)
  ;;   (term-line-mode))
  ;; (advice-add 'term :after #'my-ad-term-line-mode)
  ;; (advice-add 'ansi-term :after #'my-ad-term-line-mode)
  
    


  ;; 2048 lines of output is way too restrictive.
  (setq term-buffer-maximum-size 8192)
  :bind*
  (("C-z" . term)
   :map term-raw-map
   ("C-y" . term-paste))
)

(straight-use-package 'which-key)
(use-package which-key
  :demand t
  :diminish which-key-mode
  :bind* 
  (("C-h SPC" . which-key-show-top-level))
  :config
  (which-key-mode))

(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(straight-use-package 'elpy)
(use-package elpy
  :config

  ;; py.test is actively developed. 
  (elpy-set-test-runner `elpy-test-pytest-runner)

  ;; silences completion warning. found on ob-python's issue pages, strangely enough.
  (setq python-shell-completion-native-enable nil) 

  ;; preference
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  
  ;; start
  (elpy-enable))

(use-package kotlin-mode
  :ensure t
  :recipe (:type git :host github :repo "alphor/kotlin-mode"
           :upstream (:host github :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode")))

;; we need projectile to be loaded here, it's got a good definition
;; of vc roots
  (lsp-define-stdio-client
   kotlin-lsp-mode
   "kotlin"
   (lambda () (projectile-project-root))
   ;; timed out while waiting for a response from the language server?
   '("/usr/bin/java" "-jar" "/home/ajarara/proj/kotlin-language-server/target/KotlinLanguageServer.jar")
   )

  ;; I think the #' variant fails at compile so that we can be sure
  ;; lsp-define-stdio-client has at least generated a symbol
  (add-hook 'kotlin-mode-hook #'kotlin-lsp-mode-enable)

(use-package markdown-mode 
    :ensure t
    :recipe (markdown-mode :type git :host github :repo "alphor/markdown-mode"
                   :upstream (:host github :repo "jrblevin/markdown-mode")))

(use-package pelican-mode
  :ensure t

  :recipe (pelican-mode :type git :host github :repo "alphor/pelican-mode"
                 :upstream (:host github :repo "qdot/pelican-mode")))

(use-package indium
  :disabled t
  :config
  ;; delete all jsm modes..
  ;; I wonder what disqualifies a mode from being applicable to the environment.
  ;; (setq auto-mode-alist (assq-delete-all "\\.jsm?\\'" auto-mode-alist))
  ;; make js2-mode (javascript-IDE) the default
  ;; (setq auto-mode-alist (add-to-list '("\\.jsm?\\'" . js2-mode) auto-mode-alist))
  (add-hook 'js2-mode-hook 'indium-interaction-mode))
(setq indium-chrome-executable "chromium-browser")

(use-package circe
  :ensure t
  :recipe (circe :type git :host github :repo "alphor/circe"
                 :upstream (:host github :repo "jorgenschaefer/circe"))

:config
(setq circe-network-defaults nil)

(setq circe-network-options
      (let ((server-passwd (lambda (server-name)
                         (read-passwd
                          (format "Password for server: %s? " server-name)))))
          `(("ZNC/freenode"
         :tls t
         :host "jarmac.org"
         :port 5013
         :user "alphor/freenode"
         ;; the param is needed otherwise error!
         ;; read from minibuffer doesn't use named arguments, but has 7 of them.
         :pass ,server-passwd)
         ("ZNC/mozilla"
          :tls t
          :host "jarmac.org"
          :port 5013
          :user "alphor/mozilla"
          :pass ,server-passwd)
         ("ZNC/snoonet"
          :tls t
          :host "jarmac.org"
          :port 5013
          :user "alphor/snoonet"
          :pass ,server-passwd)
         ("ZNC/gitter"
          :tls t
          :host "jarmac.org"
          :port 5013
          :user "alphor/gitter"
          :pass ,server-passwd)
         ("local/i2p"
          :tls t
          :host "localhost"
          :port 6668)
         ;; doesn't look that interesting anymore. Maybe later.
         ;; ("ZNC/rizon"
         ;;  :tls t
         ;;  :host "jarmac.org"
         ;;  :port 6697
         ;;  :user "alphor/rizon"
         ;;  :pass (lambda (server-name) (read-passwd "Password?: ")))
         )))

;; enable nicks
(enable-circe-color-nicks)

(add-hook 'circe-mode-hook 'my-font-lock-ensure-function-nilify)

(setq tracking-ignored-buffers '(((lambda (buf-name)
                                    (not (or (string-prefix-p "#emacs" buf-name)
                                             (not (string-prefix-p "#" buf-name)))))
                                  circe-highlight-nick-face)))

;; (defadvice circe-command-SAY (after jjf-circe-unignore-target)
;;   (let ((ignored (tracking-ignored-p (current-buffer) nil)))
;;     (when ignored
;;       (setq tracking-ignored-buffers
;;             (remove ignored tracking-ignored-buffers))
;;       (message "This buffer will now be tracked."))))
;; (ad-activate 'circe-command-SAY)

(setq circe-reduce-lurker-spam t)

(defun my-circe-intersect-nicks (buf1 buf2)
    "Does what you think it does. It would make a little sense to remove your own nick from this list, but meh"
    (interactive "b\nb")
    (let ((names1 (with-current-buffer (set-buffer buf1)
                    (circe-channel-nicks)))
          (names2 (with-current-buffer (set-buffer buf2)
                    (circe-channel-nicks))))
      (message (prin1-to-string (-intersection names1 names2)))))
)

(straight-use-package
 '(circe-actions :type git :host github :repo "alphor/circe-actions"))

(use-package circe-actions)
(use-package circe-znc)

(defvar circe-actions-inspect-arg-list '()
  "A list of variables that were passed to circe-actions-inspect-args.")
(defun circe-actions-inspect-args (&rest args)
  "A utility function designed to show you what is passed to an
  arbitrary handler. Was very useful when inspecting, so I thought
  I'd leave it in here. Be warned with 30+ channels
  circe-actions-inspect-arg-list grows mighty fast, if you're adventerous
  and use circe-actions-t as a condition-function-p"
  (setq circe-actions-inspect-arg-list (cons args circe-actions-inspect-arg-list))
  (message
   (with-temp-buffer
     (cl-prettyprint args)
     (buffer-string)
     )))

(straight-use-package
  '(nix-mode :type git :host github :repo "alphor/nix-mode"
             :upstream (:host github :repo "NixOS/nix-mode")))
(use-package nix-mode)

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; persistent bookmarks
(setq bookmark-save-flag 1) ; so save after every bookmark made.

;; simple scrolling
(progn
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)
  )

;; Directory clutter
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(setq split-height-threshold nil)
;; tried 150, I'm using xfwm4 if that makes any difference, but it did not work.
(setq split-width-threshold 140)

;; here's an example, I no longer use it.
(setq safe-local-variable-values
      '((python-shell-interpreter .  "/home/ajarara/proj/viz/repl.nix")
        (python-shell-interpreter .  "/home/ajarara/proj/webkov/shell.nix")))

(use-package sx :ensure t)

(setq x-select-enable-clipboard-manager nil)

;; (setq custom-file "/dev/null")

(setq-default indent-tabs-mode nil)

;; (load-theme 'misterioso t)
(use-package monokai-theme
  :ensure t
  :recipe (monokai-theme :type git :host github :repo "alphor/monokai-emacs"
                 :upstream (:host github :repo "oneKelvinSmith/monokai-emacs"))
  :config
  (setq monokai-comments "chocolate")
  (load-theme `monokai t))

(straight-use-package 'projectile)
(use-package projectile
  :demand t
    :bind*
    (("C-c c" . projectile-compile-project)
     ("C-c f" . projectile-find-file))
    :config
    (setq projectile-completion-system 'ivy))

;; something useful from the emacs wiki? No way.
(defun my-smarter-move-beginning-of-line (arg)
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
                'my-smarter-move-beginning-of-line)

(defun my-kill-other-window ()
  (interactive)
  (if (= (count-windows) 2)
      (progn
        (other-window 1)
        (kill-buffer)
        (other-window 1))
    (error "This only works when there are two buffers!")))

;; not mine, found off of emacs-wiki. quickly switches orientation of two buffers.
(defun my-toggle-window-split ()
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

(defun my-find-init-file ()
  "Displays the contents of ~/.emacs.d/myinit.org, if already shown, revert to previous buffer"
  (interactive)
  (let ((init-file-location "/home/ajarara/.emacs.d/README.org"))
    (if (string= init-file-location (buffer-file-name))
        (previous-buffer)
      (find-file init-file-location)))
  )

(defun my-find-projects ()
  "navigates to ~/Documents/projects"
  (interactive)
  (find-file "~/Documents/projects/"))

(defun my-find-org-files ()
  "navigates to ~/Documents/org"
  (interactive)
  (find-file "~/Documents/org/"))

(defun my-font-lock-ensure-function-nilify ()
  (setq-local font-lock-ensure-function
        'ignore))

(defun my-github (query)
  (interactive "sSearch Github: ")
  (browse-url (format "https://github.com/search?q=%s" query)))

;; non obtrusive version of helm-google-suggest. Although helm-google-suggest is more fun
(defun my-google (query)
  "It's mine! MIIIIIIINE!"
  (interactive "sSearch the googs: ")
  (browse-url (format "https://google.com/#q=%s" query)))

(defun pelican-now (&optional arg)
  (interactive "P")
  (let ((date (format-time-string "%Y-%m-%d %H:%M:%S %z")))
    (if arg
        (insert date)
      (message date))))

(bind-key* "C-h" `help-command)
(bind-key* "C-h C-h" (lambda ()
    (interactive) (info "(emacs) Help Summary")))

(bind-key* "M-[" `previous-buffer)
(bind-key* "M-]" `next-buffer)

(bind-key* "M-." `xref-find-definitions-other-window)

(define-key key-translation-map (kbd "C-M-g") (kbd "C-g"))

(global-set-key (kbd "M-/") 'hippie-expand)

(bind-key* "M-q" `quoted-insert)

;; shadows universal arg, I think? Damn, I need to read the manual.
(bind-key* "C-0" `text-scale-adjust)

;; shadows capitalize word (used to be my minor mode keymap, I moved all that to evil-leader, which I may eventually move to general)
(bind-key "M-c" `comment-dwim)

;; shadows indent-new-comment-line
(bind-key* "M-j" `end-of-buffer)

;; shadows move-to-window-line-top-bottom
(bind-key* "M-r" `delete-other-windows)

;; would like this instead to just kill the buffer, or like rotate. I think I need some buffer management tool
;; shadows kill-ring-save
(bind-key* "M-w" `delete-window)

;; I don't actually know what the name of the function is, but I know I don't need it. It's some typeface stuff.
;; also, the function name here is misleading, it evaluates the whole top-level expression, from anywhere in the expression, not just defuns
;; shadows Set face:

;; I'm gonna need shackle just for this async.
;; shadows universal argument, 7
(bind-key* "M-7" `async-shell-command)

;; shadows universal argument, 1
(bind-key* "M-1" `shell-command)

;; shadows prefix containing occur
(bind-key* "M-s" 'switch-to-buffer)

;; shadows tab-to-tab-stop
(bind-key* "M-i" `my-find-init-file)

;; instantly kills buffer (without deleting the window), unless unsaved content. this advices kill-buffer
;; shadows kill-sentence
(bind-key* "M-z" `kill-this-buffer)

;; U for undeaaaaaaaaaaaaaaaaad
;; shadows upcase-word
(bind-key* "M-u" `bury-buffer)

;; shadows nothing that I know of.
;; (bind-key* "M-p" `my-find-projects)

;; this leaves M-d free, for something. Although I use mode-d for colon/semicolon
;; shadows kill-sentence
(bind-key* "M-k" `kill-word)

;; shadows nothing
(bind-key* "M-\"" `insert-pair)

;; shadows nothing
(bind-key* "<f5>" `recompile)

(add-hook `org-mode-hook `org-indent-mode)
(add-hook `org-mode-hook `visual-line-mode)

(add-hook 'apropos-mode-hook (lambda () (local-set-key (kbd "C-c f") 'apropos-follow)))

;; the <- shortcut is not helpful when you can't use hyphens in variable names

(add-hook 'ess-mode-hook (lambda () (local-set-key (kbd "_" 'self-insert-command))))

(message "Emacs config successfully loaded!")
