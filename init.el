
(toggle-debug-on-error)

;; package-archive config
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; Quelpa bootstrap
  (package-initialize)
  (if (require 'quelpa nil t)
      (quelpa-self-upgrade)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer)))

;; Use-package bootstrap
(quelpa 'use-package :stable t)
;; is this really necessary? imenu allows me to instead manage this file through the headings anyway.
(setq use-package-enable-imenu-support t)

;; bind-key is provided with use-package, diminish I only use once or twice
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(set-face-attribute 'default nil :font "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")

;; Disabling 'helpful' visual goodies
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; enabling visual line mode
(visual-line-mode 1)

(toggle-word-wrap)

;; Making emacs snappier
(fset `yes-or-no-p `y-or-n-p)
(setq echo-keystrokes 0.1)
(setq mouse-yank-at-point t)

(quelpa `swiper) ; installs both swiper and ivy
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t))

(use-package swiper
  :config

  ;; almost required, I use search a lot for navigation, especially in this growing init file. Note that if multiple candidates are in a view moving between them does not recenter the buffer.
  (setq swiper-action-recenter t)
  ;; shadows isearch
  :bind* (("C-s" . swiper))
  )

(quelpa 'ace-window)
(use-package ace-window
  :bind*
  ;; shadows fill-paragraph
  (("M-q" . ace-window)
   ;; despite quoted-insert growing on me, maybe that's better reserved for something to be used in evil-leader, <leader> q or something, as that's definitely something I'll use in normal mode often.
   ;; shadows quoted-insert
   ("C-q" . ace-window)
   ;; needs abo-abo's key config (search for "semimap")
   ;; U03A1
   ("Ρ" . ace-window))
  :config
  )

(quelpa 'magit)
(use-package magit)

(use-package evil
    ;; evil-leader is run before evil, so that leader keys work in scratch and messages

:init
 (setq evil-toggle-key "C-`")

(setq evil-want-fine-undo t)

(quelpa 'evil-leader)
(use-package evil-leader
  :config
  (setq evil-leader/leader "<SPC>")

  (evil-leader/set-key "g" `keyboard-quit)
  (evil-leader/set-key "C-g" `keyboard-quit)

  (evil-leader/set-key "SPC" `ace-window)

  (evil-leader/set-key "w" `save-buffer)
  (evil-leader/set-key "v" `visual-line-mode)
  (evil-leader/set-key "t" `toggle-word-wrap)
  (evil-leader/set-key "s" `magit-status)

  (evil-leader/set-key "f" `find-file)
  (evil-leader/set-key "p" `my/find-projects)
  (evil-leader/set-key "o" `my/find-org-files)

  (evil-leader/set-key "r" `org-capture)
  (global-evil-leader-mode)
  )

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

(quelpa 'evil-visual-mark-mode)
(use-package evil-visual-mark-mode
  :config
  (evil-visual-mark-mode))

:demand t

:bind* (:map evil-emacs-state-map
             ("C-r" . evil-paste-from-register)
             :map evil-normal-state-map
             ("j" . evil-next-visual-line)
             ("k" . evil-previous-visual-line)
             ("'" . evil-goto-mark)
             ("C-y" . yank))

:bind-keymap*
  (("C-w" . evil-window-map))
)

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
          ("p" "programming-lang" entry (file+datetree "~/Documents/org/pl.org")
           "* %?\nEntered on %U\n  %i")
          )
        )
  :bind*
  (("<f5>" . org-capture))
  )

(use-package term
  ;; ugh, I need a good terminal emulator. I only use an emacs term over real ones because I get to use evil (or emacs keys, if you're that kinda guy)
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
  (("C-z" . term)
   :map term-raw-map
   ("C-h" . help-command)
   ("C-y" . term-paste))

)

(quelpa 'which-key)
(use-package which-key
  :demand t
  :diminish which-key-mode
  :bind* 
  (("C-h SPC" . which-key-show-top-level))
  :config
  (which-key-mode))

(quelpa 'helm)
(use-package helm
  :init
  ;; helm sets this stuff off, and they're not gonna fix it: https://github.com/emacs-helm/helm/issues/1498#issue-154021209
  (setq ad-redefinition-action 'accept)
  :ensure t
  :bind* (("M-x" . helm-M-x)))

(quelpa 'mingus)
(use-package mingus)

(quelpa 'slime)
(use-package slime
  :config
  (slime-setup)
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(quelpa 'circe)
(use-package circe
  :config
  (setq circe-split-line-length 200)
  (setq circe-reduce-lurker-spam t)
  (setq circe-network-options
        '(("ZNC"
           :tls t
           :host "jarmac.org"
           :port 6697
           :user "alphor"
           ;; the param is needed otherwise error!
           ;; read from minibuffer doesn't use named arguments, but has 7 of them.
           :pass (lambda (server-name) (read-passwd "Password?: "))
           ))))

(setq server-use-tcp t)

;; persistent bookmarks
(setq bookmark-save-flag 1) ; so save after every bookmark made.

;; simple scrolling
(progn
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)
  )

(quelpa 'expand-region)
(use-package expand-region
  :bind (("M-t" . er/expand-region))
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

(setq x-select-enable-clipboard-manager nil)

;; (load-theme 'misterioso t)
(quelpa `monokai-theme)
(use-package monokai-theme
  :config
  (load-theme `monokai t))

(quelpa 'try)
(use-package try)

(quelpa 'sml-mode)
(use-package try)

(quelpa 'ledger-mode)
(use-package ledger-mode
  :config
  (autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
  (add-to-list 'load-path
               (expand-file-name "/path/to/ledger/source/lisp/"))
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

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
  "Displays the contents of ~/.emacs.d/myinit.org, if already shown, revert to previous buffer"
  (interactive)
  (let ((init-file-location "/home/ajarara/.emacs.d/myinit.org"))
    (if (string= init-file-location (buffer-file-name))
        (previous-buffer)
      (find-file init-file-location)))
  )

(defun my/find-projects ()
  "navigates to ~/Documents/projects"
  (interactive)
  (find-file "~/Documents/projects/"))

(defun my/find-org-files ()
  "navigates to ~/Documents/org"
  (interactive)
  (find-file "~/Documents/org/"))

(define-key key-translation-map [?\C-h] [?\C-p])
(define-key key-translation-map [?\C-p] [?\C-h])

;; if there are two letters commented after the definition, the second is reached by using shift AND mode shift. It's a lot, so don't expect there to be many
;; movement
(define-key key-translation-map "ν" (kbd "M-f")) ;; [f]
(define-key key-translation-map "β" (kbd "M-b")) ;; [b]

;; shortcuts
(define-key key-translation-map "Ι" (kbd "M-i")) ;; [i]
(define-key key-translation-map "Σ" (kbd "M-z")) ;; [z]
(define-key key-translation-map "χ" (kbd "M-c")) ;; [c]

;; window manipulation
(define-key key-translation-map "ψ" (kbd "M-r")) ;; [r]

;; shadows universal arg, I think? Damn, I need to read the manual.
(bind-key* "C-0" `text-scale-adjust)

;; shadows capitalize word (used to be my minor mode keymap, I moved all that to evil-leader, which I may eventually move to general)
(bind-key "M-c" `comment-dwim)

;; shadows move-to-window-line-top-bottom
(bind-key* "M-r" `delete-other-windows)

;; would like this instead to just kill the buffer, or like rotate. I think I need some buffer management tool
;; shadows kill-ring-save
(bind-key* "M-w" `delete-window)

;; I don't actually know what the name of the function is, but I know I don't need it. It's some typeface stuff.
;; also, the function name here is misleading, it evaluates the whole top-level expression, from anywhere in the expression, not just defuns
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
(bind-key* "M-z" `kill-this-buffer)

;; U for undeaaaaaaaaaaaaaaaaad
;; shadows upcase-word
(bind-key* "M-u" `bury-buffer)

;; shadows nothing that I know of.
(bind-key* "M-p" `my/find-projects)

(add-hook `org-mode-hook `org-indent-mode)
(add-hook `org-mode-hook `visual-line-mode)

;; disable debugging
(toggle-debug-on-error)

(message "Emacs config successfully loaded!")
