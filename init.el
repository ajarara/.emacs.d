(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; load up the profile. This is an untracked file that sets
;; `profile' to either `personal-macOS', `personal-guix'.
(let ((profile-path (concat user-emacs-directory "profile.el")))
  (if (file-exists-p profile-path)
      (load profile-path)
    (defvar profile 'nil)))

(defvar is-personal-profile
  (string-prefix-p "personal" (symbol-name profile)))

(use-package project)

(use-package meow
  :demand t
  :config
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore))
    (meow-global-mode))
(use-package diminish)

(use-package magit)

(use-package key-chord
  :config
  ;; hmm.. when grabbing this key chord is not respected
  (key-chord-define
   meow-insert-state-keymap "jj" 'meow-normal-mode)
  (key-chord-mode))

(use-package general
  :demand t
  :config
  (general-define-key
   "g" 'keyboard-quit
   "C-g" 'keyboard-quit
   "SPC" 'ace-window

   "w" 'save-buffer
   "v" 'visual-line-mode
   "t" 'toggle-word-wrap
   
   "a" 'counsel-ag
   
   "m" 'fill-region

   "f" 'projectile-find-file
   "c" 'projectile-compile-project
   
   "p" 'my-find-projects
   "o" 'my-find-org-files

   "r" 'org-capture

   "i" 'imenu
   :prefix "C-c"))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package ace-window
  :bind*
  (("C-t" . ace-window))
  :config
  (setq aw-scope 'frame))

(use-package password-store)

(use-package circe
  :requires password-store
  :config
  (setq circe-network-defaults nil)
  (setq lui-fill-column 63)
  
  (setq circe-network-options
        `(("libera"
           :tls t
           :host "irc.libera.chat"
           :port 6697
           :sasl-strict t
           :sasl-username "ajarara"
           :sasl-password (lambda (host)
                            (password-store-get host))
           )))
    ;; I have no idea why colored nicks are not enabled by default. Much
    ;; prettier! (This is the default option I was complaining about
    ;; earlier)
    (enable-circe-color-nicks)
    
    ;; Unfortunately, swiper calls font-lock-ensure-function which has
    ;; the annoying habit of washing out all the color. I add a
    ;; function to circe's mode hook that sets font-lock-ensure to the
    ;; ignore function.
    (add-hook 'circe-mode-hook
              (lambda ()
                (setq-local font-lock-ensure-function 'ignore)))

    ;; Don't bombard me with leaves if the leaver hasn't spoke in a while.
    (setq circe-reduce-lurker-spam t)

    (defun my-circe-intersect-nicks (buf1 buf2)
      "Does what you think it does. It would make a little sense to remove your own nick from this list, but meh"
      (interactive "b\nb")
      (let ((names1 (with-current-buffer (set-buffer buf1)
                      (circe-channel-nicks)))
            (names2 (with-current-buffer (set-buffer buf2)
                      (circe-channel-nicks))))
        (message (prin1-to-string (-intersection names1 names2))))))

(straight-use-package
 '(circe-actions :type git :host github :repo "alphor/circe-actions"))
(use-package circe-actions)

(when is-personal-profile
  (use-package ag)
  (use-package counsel
    :bind* (("M-x" . counsel-M-x)
            ("C-c a" . counsel-ag))))

(defvar backup-directory
  (concat user-emacs-directory "backup"))

(make-directory backup-directory t)
(setq backup-directory-alist `((".*" . ,backup-directory)))
(setq version-control t)
(setq delete-old-versions t)

(defvar autosave-directory
  (concat user-emacs-directory "autosave"))
(make-directory autosave-directory t)
(setq auto-save-list-file-prefix autosave-directory)
(setq auto-save-file-name-transforms `((".*" ,autosave-directory t)))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'direnv-mode)
  (add-to-list 'auto-mode-alist'("\\.go" . go-mode)))

(use-package guix
  :config
  (setq guix-dot-program "xt"))

;; https://www.reddit.com/r/emacs/comments/qeehqa/is_there_a_way_to_highlight_the_content_inside/hhsfsry/
(use-package paren
  :config
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook
  (after-init-hook . show-paren-mode))

(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(use-package markdown-mode)

(use-package org
  :config
  (setq org-directory "~/notes/org/")

  (setq org-default-notes-file (concat org-directory "sink.org"))
  (setq org-capture-templates
        (cond
         ((eq profile 'personal-guix)
          `(("j"
             "journal"
             entry
             (file+datetree ,(concat org-directory "journal-second.org"))
             "* %?\nEntered on %U\n  %i\n  %a")
            ("t"
             "todo"
             entry
             (file+datetree ,(concat org-directory "todo.org"))
             "* TODO %?\n  %i\n  %a")))
         (t nil)))
  (add-hook `org-mode-hook `org-indent-mode)
  (add-hook `org-mode-hook `visual-line-mode))


;; Prefer horizontal splits when the frame has the space for it.
;; By horizontal, I mean vim's and the rest of the world's notion of vertical.

;; You split along the horizontal axis, I guess. Sure.
(setq split-height-threshold nil)
(setq split-width-threshold 140)
(setq-default cursor-type 'hbar)

(setq-default indent-tabs-mode nil)

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

;; maybe the only legitimate use of global-set-key here.
(global-set-key [remap move-beginning-of-line]
                'my-smarter-move-beginning-of-line)

(use-package pinentry
  :disabled t
  :config
  (pinentry-start))

(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(bind-key* "C-h" `help-command)
(bind-key* "C-h C-h" (lambda ()
    (interactive) (info "(emacs) Help Summary")))

(use-package xref
  :config
  (setq xref-show-definitions-function 'xref--show-defs-buffer-at-bottom)
  (bind-key* "M-." `xref-find-definitions-other-window))

(define-key key-translation-map (kbd "C-M-g") (kbd "C-g"))

(bind-key* "C-0" `text-scale-adjust)

(bind-key "M-c" `comment-dwim)

(bind-key* "M-j" `end-of-buffer)

(bind-key* "M-7" `async-shell-command)

(bind-key* "M-1" `shell-command)

(bind-key* "M-s" 'switch-to-buffer)

(bind-key* "M-i"
           (lambda ()
             (interactive)
             (let ((init-file-location (concat user-emacs-directory "init.el")))
               (if (string= init-file-location (buffer-file-name))
                   (previous-buffer)
                 (find-file init-file-location)))))

(use-package projectile
  :disabled t
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "package.json")
  (setq projectile-completion-system 'ivy))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package rust-mode
  :mode "\\.rs"
  :config
  (add-hook 'rust-mode-hook 'lsp))

(use-package company)

(use-package tide
  :config
  (setq typescript-indent-level 2)
  (defun tide-setup-hanger ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)

    (tide-hl-identifier-mode 1)
    (local-set-key (kbd "M-?") 'tide-references)
    (company-mode 1))
  (add-hook 'typescript-mode-hook 'tide-setup-hanger))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))
  (setq typescript-indent-level 2)
  (setq js-indent-level 2))


(defun node-repl ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1") ;avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; just type y or n without hitting enter
(fset `yes-or-no-p `y-or-n-p)

(setq echo-keystrokes 0.1)
(setq mouse-yank-at-point t)
(setq vc-follow-symlinks nil)
(setq disabled-command-function nil)

(use-package ivy
  :demand t
  :diminish ivy-mode
  :config
  (setq ivy-ignore-buffers `("\\` "))
  
  ;; i like completion in the minibuffer, completion in region is obnoxious when you have hl-line-mode active. This must be set before ivy-mode is called.
  (setcdr (assoc 'ivy-completion-in-region ivy-display-functions-alist) nil)

  (ivy-mode t))

(use-package swiper
  :config
  (setq swiper-action-recenter t)

  ;; shadows isearch
  :bind* (("C-s" . swiper)))

  ;; why is this not the default? 
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)

;; (use-package monokai-theme
;;   :config
;;   (setq monokai-comments "chocolate")
;;   (load-theme `monokai t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; horizontal scrolling bad
(visual-line-mode 1)

(column-number-mode)

;; pretty quotes can't be jumped to easily.
    (setq text-quoting-style 'grave)

(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(dolist (this-mode-hook `(prog-mode-hook
                          circe-mode-hook))
  (add-hook this-mode-hook `hl-line-mode))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :bind* 
  (("C-h SPC" . which-key-show-top-level))
  :config
  (which-key-mode))

(use-package git-link)

(when (eq profile 'personal-macOS)
  (setq ring-bell-function 'ignore))

(use-package geiser
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/src/guix")
    (add-to-list 'geiser-guile-load-path "~/src/nonguix")))

(use-package geiser-guile)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package deft
  :init
  (setq deft-extension "gpg"))

 (use-package srfi
   :config
   (add-hook
    'srfi-mode-hook
    (lambda ()
      (setq-local browse-url-browser-function 'eww))))
(setq ns-right-command-modifier 'control)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((geiser-default-implementation quote guix)
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (expand-file-name root-dir-unexpanded))
                  (root-dir*
                   (directory-file-name root-dir)))
               (unless
                   (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
