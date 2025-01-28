;; -*- lexical-binding: t -*-
(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar my-lisp (expand-file-name "lisp/" user-emacs-directory)
  "Added to load path. Also holds custom.el")

(add-to-list 'load-path my-lisp)

(require 'cl-lib)
(require 'json)

(defvar attributes
  '(is-personal has-guix has-org has-magit has-self)
  "Attributes are minor modes that are enabled on a per host basis")
(cl-loop for attribute in attributes
         do (eval `(define-minor-mode ,attribute nil :global t)))

;; convenience wrapper that adds a hook to the profile
;; attribute. After adding, it runs the hook. It otherwise behaves
;; like add-hook, calling on teardown, which means uses should still
;; inspect whether the minor mode is enabled and do teardown if needed
(defmacro subscribe-to-attribute (attribute &rest body)
  (declare (indent defun))
  (cl-assert (memq attribute attributes))
  `(progn
     (add-hook ',(intern (format "%s-hook" attribute)) (lambda () ,@body))
     ,@body
     nil))

(require 'profile nil t)
(if (boundp 'profile)
    (cl-loop for attr in profile
             do (unless (memq attr attributes)
                  (error "unrecognized attribute from profile: %s" attr))
             do (funcall attr)))

(when (and
       has-self
       (not (file-exists-p "~/self")))
  (display-warning :error "Clone the self repository!!"))

(defmacro use-package-conditionally (name mode &rest body)
  "See https://github.com/radian-software/straight.el/issues/235. This makes it so that we don't clone if we're never going to use it, but the recommendation is to still register the package for... reasons. Eventually we will be able to move to :if exprs once the issue is resolved. The reason we do this is because often I boot up emacs on new machines, and less clones means way faster startup."
  (declare (indent defun))
  `(subscribe-to-attribute ,mode
     (if ,mode
         (use-package ,name ,@body)
       (straight-register-package ',name))))

(use-package general
  :demand t
  :config
  (general-define-key
   "C-z" (make-sparse-keymap))
                  
  (general-define-key
   "C-z" 'compile
   "C-r" 'recompile
   "C-a" 'async-shell-command
   "C-k" 'bury-buffer
   :prefix "C-z")
  (general-define-key
   "g" 'keyboard-quit
   "C-g" 'keyboard-quit

   "w" 'save-buffer
   "v" 'visual-line-mode
   "t" 'toggle-word-wrap
   
   "m" 'fill-region

   "i" 'imenu
   :prefix "C-c"))

(use-package project
  :config
  (general-define-key
   "a" 'project-async-shell-command
   :keymaps 'project-prefix-map))
(use-package diminish)
(use-package-conditionally magit has-magit)
(use-package markdown-mode)
(use-package company)
(use-package git-link)

(use-package-conditionally buttercup is-personal)

(use-package ansi-color
  :config
  (defun my-colorize-buffer-compilation-hook ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'my-colorize-buffer-compilation-hook))

(use-package desktop-environment
  :config
  (let ((screenies (expand-file-name "~/screenies")))
    (unless (file-exists-p screenies)
      (mkdir screenies))
    (setq desktop-environment-screenshot-directory screenies)))

(use-package-conditionally org has-org
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1))))

(use-package winner
  :after general
  :config
  (general-define-key
   "M-I" 'winner-undo
   "M-O" 'winner-redo)
  (winner-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package expand-region
  :after general
  :config
  (general-define-key
   "C-\\" 'er/expand-region))

(use-package ace-window
  :after general
  :config
  (general-define-key
   "C-SPC" 'ace-window
   "C-o" 'ace-window
   "o" 'ace-window
   :prefix "C-c")
  (general-define-key "M-o" 'ace-window :keymaps '(general-override-mode-map))
  (general-define-key
   "C-o" 'ace-window
   "o" 'ace-window
   :prefix "C-x")
  (setq aw-keys
        '(?j ?k ?l ?\; ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
  (setq aw-scope 'frame))

(use-package avy
  :after general
  :config
  (general-define-key
   "M-l" 'avy-goto-char-timer))

(use-package-conditionally ag is-personal)

(use-package-conditionally direnv is-personal
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after (vertico general)
  :config
  (general-define-key "C-s" 'consult-line)
  (general-define-key "C-S-s" 'consult-line-multi)
  (general-define-key "C-h a" 'consult-apropos))

(use-package-conditionally guix has-guix
  :config
  (setq guix-dot-program "xt"))

(use-package paren
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook
  (after-init-hook . show-paren-mode))

(use-package ibuffer
  :after general
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

(use-package-conditionally rust-mode is-personal
  :mode "\\.rs"
  :config
  (eval-when-load
   "lsp-mode"
   (add-hook 'rust-mode-hook 'lsp)))

(use-package-conditionally cargo is-personal
  :after 'rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package-conditionally typescript-mode is-personal
  :config
  (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))
  (setq typescript-indent-level 2)
  (setq js-indent-level 2))

(use-package monokai-theme
  :config
  (setq monokai-comments "chocolate")
  (load-theme `monokai t))

(use-package which-key
  :demand t
  :after general
  :diminish which-key-mode
  :config
  (general-define-key
   "SPC" 'which-key-show-top-level
   :prefix "C-h")
  (which-key-mode))

(use-package-conditionally geiser is-personal
  :config
  (setq geiser-repl-history-filename
        (or (file-name-concat (getenv "XDG_STATE_HOME") ".geiser_history")
            geiser-repl-history-filename)))

(use-package-conditionally geiser-guile is-personal
  :after geiser
  :config
  (add-to-list 'geiser-guile-load-path "~/upstream/nonguix")
  ;; (add-to-list 'geiser-guile-load-path "~/src/guix")
  ;; (remove "~/src/guix" geiser-guile-load-path)
  (subscribe-to-attribute has-guix
    (let ((emacs-bin (expand-file-name "bin" user-emacs-directory)))
      (cond
       (has-guix
        ;; quick hack to get us channel/profile awareness
        (setq geiser-guile-binary (list "guix" "repl"))
        (cl-pushnew emacs-bin exec-path :test #'equal))
       ((not has-guix)
        (setq geiser-guile-binary "guile"))))))

(use-package-conditionally paredit is-personal
  :config
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package-conditionally srfi is-personal
  :config
  (add-hook
   'srfi-mode-hook
   (lambda ()
     (setq-local browse-url-browser-function 'eww))))

(use-package cc-mode
  :config
  (add-hook
   'c-mode-hook
   (lambda ()
     (setq-local
      compile-command
      "make -k -j $(nproc --ignore=2)"))))

(use-package-conditionally ement is-personal
  :requires auth-source
  :config
  (defun my-ement-connect-matrix ()
    (interactive)
    (let* ((search-results
            (auth-source-search :host "matrix.org" :max 1))
           (_ (cl-assert search-results t "No password found for matrix!"))
           (auth-info (car search-results))
           (user (plist-get auth-info :user))
           (password (auth-info-password auth-info))
           (user-id (concat "@" user ":matrix.org")))
      (ement-connect :user-id user-id :password password))))  

(use-package-conditionally nov.el is-personal
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package-conditionally dumb-jump is-personal
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package tui
  :config
  (require 'tui-use-process-buffer)
  (add-hook 'kill-buffer-hook #'tui-unmount-current-buffer-content-trees)
  :straight
  '(:host github :repo "ajarara/tui.el" :branch "ajarara/add-use-effect-state" :files ("*.el" "components" "layout" "demo" "snippets")))

(use-package flycheck)

;; https://github.com/casouri/tree-sitter-module has a bunch of them installed
(use-package tree-sitter)

(use-package kotlin-mode)

(use-package-conditionally auth-source has-self
  :config
  (push "~/self/vault/.authinfo.gpg" auth-sources))

(use-package-conditionally circe has-self
  :requires auth-source
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
                            (let* ((search-result
                                    (auth-source-search :host "irc.libera.chat"))
                                   (auth-info
                                    (car search-result)))
                              (auth-info-password auth-info))))))
  (enable-circe-color-nicks)
    
  ;; Don't bombard me with leaves if the leaver hasn't spoke in a while
  (setq circe-reduce-lurker-spam t)

  (defun my-circe-intersect-nicks (buf1 buf2)
    "Does what you think it does. It would make a little sense to remove your own nick from this list, but meh"
    (interactive "b\nb")
    (let ((names1 (with-current-buffer buf1
                    (circe-channel-nicks)))
          (names2 (with-current-buffer buf2
                    (circe-channel-nicks))))

      (message (prin1-to-string (-intersection names1 names2))))))

(use-package-conditionally nov is-personal
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package yaml-mode)

(use-package sops)

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package emacs
  :after general
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

If ARG is not nil or 1, move forward ARG - 1 lines first.  Ifpoint reaches the beginning or end of the buffer, stop there."
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

    (general-define-key [remap move-beginning-of-line]
                        'my-smarter-move-beginning-of-line))

  (defalias 'pretty-print-last-sexp 'pp-macroexpand-last-sexp)

  (progn
    (defun my-toggle-init ()
      (interactive)
      (let ((init-file-location
             (file-truename
              (concat user-emacs-directory "init.el")))
            (current-location
             (and buffer-file-name
                  (file-truename buffer-file-name))))
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
   "M-s" 'switch-to-buffer)

  (defun node-repl ()
    (interactive)
    (setenv "NODE_NO_READLINE" "1") ;avoid fancy terminal codes
    (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

  ;; just type y or n without hitting enter
  (fset `yes-or-no-p `y-or-n-p)

  (setq echo-keystrokes 0.1)
  (setq mouse-yank-at-point t)
  (setq vc-follow-symlinks nil)
  (setq disabled-command-function nil)

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
    (add-hook this-mode-hook `hl-line-mode))

  (add-hook 'shell-mode-hook 'read-only-mode)
  (setq ring-bell-function 'ignore)

  (setq ns-right-command-modifier 'control))

(setq custom-file (concat my-lisp "custom.el"))
(load custom-file)
