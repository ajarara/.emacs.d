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

(use-package diminish)
(use-package magit)
(use-package key-chord)
(use-package markdown-mode)
(use-package company)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

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


(use-package ace-window
  :config
  (setq aw-scope 'frame))

(use-package password-store
  :disabled (not is-personal-profile))

(use-package ag
  :disabled (not is-personal-profile))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'direnv-mode)
  (add-to-list 'auto-mode-alist'("\\.go" . go-mode)))

(use-package guix
  :disabled (not is-personal-profile)
  :config
  (setq guix-dot-program "xt"))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook
  (after-init-hook . show-paren-mode))

(use-package ibuffer
  :config
  (general-define-key
   "C-b" 'ibuffer
   :prefix "C-x"))

(use-package xref
  :config
  (setq xref-show-definitions-function 'xref--show-defs-buffer-at-bottom))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; tie it all together
(use-package emacs
  :config
  (let ((backup-directory (concat user-emacs-directory "backup")))
    (make-directory backup-directory t)
    (setq backup-directory-alist `((".*" . ,backup-directory)))
    (setq version-control t)
    (setq delete-old-versions t))
  (let ((auto-save-directory (concat user-emacs-directory "autosave")))
    (make-directory auto-save-directory t)
    (setq auto-save-list-file-prefix auto-save-directory)
    (setq auto-save-file-name-transforms `((".*" ,auto-save-directory t))))
  (progn
    ;; Prefer horizontal splits when the frame has the space for it.
    ;; By horizontal, I mean vim's and the rest of the world's notion
    ;; of vertical.

    ;; You split along the horizontal axis, I guess. Sure.
    (setq split-height-threshold nil)
    (setq split-width-threshold 140))
  (setq-default cursor-type 'hbar)
  (setq-default indent-tabs-mode nil)

  (progn
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
    (general-define-key [remap move-beginning-of-line]
                    'my-smarter-move-beginning-of-line))

  (progn
    (defun my-toggle-init ()
      (interactive)
      (let ((init-file-location
             (file-truename
              (concat user-emacs-directory "init.el")))
            (current-location
             (file-truename (buffer-file-name))))
        (if (string= init-file-location current-location)
            (previous-buffer)
          (find-file init-file-location))))
    (general-define-key "M-i" 'my-toggle-init))

  (progn
    (setq scroll-conservatively 10000)
    (setq auto-window-vscroll nil))


  (general-define-key
   "M-0" 'text-scale-adjust
   "M-1" 'shell-command
   "M-7" 'async-shell-command
   "M-s" 'switch-to-buffer))

;; ------ BANKRUPTCY LINE ----------
;; shouldn't be in here: We should just use general to write keybinds.
(require 'bind-key)

(use-package lsp-mode 
  :disabled (not is-personal-profile)
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package circe
  :disabled (not is-personal-profile)
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

;; (straight-use-package
;;  '(circe-actions :type git :host github :repo "alphor/circe-actions"))
;; (use-package circe-actions)


(use-package counsel
  :disabled (not is-personal-profile)
  :bind* (("M-x" . counsel-M-x)
          ("C-c a" . counsel-ag)))
                 
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


(use-package rust-mode
  :mode "\\.rs"
  :config
  (add-hook 'rust-mode-hook 'lsp))

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

(use-package monokai-theme
  :config
  (setq monokai-comments "chocolate")
  (load-theme `monokai t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; horizontal scrolling bad
(visual-line-mode 1)

(column-number-mode)

(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(dolist (this-mode-hook `(prog-mode-hook
                          circe-mode-hook))
  (add-hook this-mode-hook `hl-line-mode)
  (general-define-key "M-c" 'comment-dwim))

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
  :disabled (not is-personal-profile)
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/src/guix")
    (add-to-list 'geiser-guile-load-path "~/src/nonguix")))

(use-package geiser-guile
  :disabled (not is-personal-profile))

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
