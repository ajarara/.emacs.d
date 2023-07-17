;; -*- lexical-binding: t -*-

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(require 'cl-lib)
(require 'json)

(defvar attributes
  '(is-personal is-guix)
  "Attributes are minor modes that are enabled on a per host basis
(and in some cases can be flipped on or off interactively)")

(cl-loop for attribute in attributes
         do (eval `(define-minor-mode ,attribute nil :global t)))

;; convenience wrapper that adds a hook to the profile attribute. After adding,
;; if the profile attribute mode is enabled, it runs the hook. It otherwise
;; behaves like add-hook, calling on teardown, which means uses
;; should still inspect  whether the minor mode is enabled and do
;; teardown if needed
(defmacro subscribe-to-attribute (attribute &rest body)
  (declare (indent defun))
  (cl-assert (memq attribute attributes))
  `(progn
     (add-hook ',attribute (lambda () ,@body))
     ,(when attribute
        `(progn ,@body nil))))

;; load up the profile
(let ((profile-path (concat user-emacs-directory "profile.json")))
  (if (file-exists-p profile-path)
      (let ((parsed (json-read-file profile-path)))
        (cl-assert (vectorp parsed))
        (cl-loop for enabled-attr-str across parsed
                 for interned-attr = (intern enabled-attr-str)
                 do (unless (memq interned-attr attributes)
                      (error "unrecognized attribute from profile: %s"))
                 do (funcall interned-attr)))))

(use-package general
  :demand t
  :config
  (general-define-key
   "g" 'keyboard-quit
   "C-g" 'keyboard-quit

   "w" 'save-buffer
   "v" 'visual-line-mode
   "t" 'toggle-word-wrap
   
   "m" 'fill-region

   "i" 'imenu
   :prefix "C-c"))

(use-package project :elpaca nil)
(use-package diminish)
(use-package magit)
(use-package markdown-mode)
(use-package company)
(use-package git-link)
(use-package buttercup)



(use-package desktop-environment
  :config
  (let ((screenies (expand-file-name "~/screenies")))
    (unless (file-exists-p screenies)
      (mkdir screenies))
    (setq desktop-environment-screenshot-directory screenies)))
        

(use-package org :elpaca nil)

(use-package compile
  :elpaca nil
  :config
  ;; https://stackoverflow.com/a/20788581 more or less
  (when is-personal
    (ignore-errors
      (require 'ansi-color)
      
      (defun my-colorize-compilation-buffer ()
        (when (eq major-mode 'compilation-mode)
          (ansi-color-compilation-filter)))
      (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))))

(use-package winner
  :elpaca nil
  :after general
  :config
  (general-define-key
   "M-I" 'winner-undo
   "M-O" 'winner-redo)
  (winner-mode))

(use-package savehist
  :elpaca nil
  :config
  (savehist-mode))

(use-package async-await
  :config
  (promise-rejection-tracking-enable '((all-rejections . t))))

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

(use-package password-store
  :if is-personal)

(use-package ag
  :if is-personal)

(use-package direnv
  :if is-personal
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

(use-package guix
  :if is-guix
  :config
  (setq geiser-repl-company-p nil) ; geiser removed in https://gitlab.com/emacs-geiser/geiser/-/merge_requests/7
  (defalias 'geiser-company--setup 'ignore)

  (defvar my-manifest-path (expand-file-name "~/self/home/installed-packages.scm"))
  (async-defun my-sync-manifest-after-operation ()
    (let* ((current-md5-of-manifest
           (car
            (split-string
             (car (await (promise:make-process `("md5sum" ,my-manifest-path))))
             " ")))
           (manifest-export (car (await (promise:make-process '("guix" "package" "--export-manifest")))))
           (new-md5-of-manifest (md5 manifest-export)))
      (if (not (equal current-md5-of-manifest new-md5-of-manifest))
          (with-temp-buffer
            (insert manifest-export)
            (write-file my-manifest-path)))))
  
  (add-hook 'guix-repl-after-operation-hook 'my-sync-manifest-after-operation)
  (async-defun my-guix-update-all ()
    (interactive)
    (await (promise:make-process '("guix" "pull")))
    (await (promise:make-process `("guix" "package" "-m" ,my-manifest-path))))
            
  (setq guix-dot-program "xt"))

(use-package paren
  :elpaca nil
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook
  (after-init-hook . show-paren-mode))

(use-package ibuffer
  :elpaca nil
  :after general
  :config
  (general-define-key
   "C-b" 'ibuffer
   :prefix "C-x"))

(use-package xref
  :config
  (setq xref-show-definitions-function 'xref--show-defs-buffer-at-bottom))

(use-package re-builder
  :elpaca nil
  :config
  (setq reb-re-syntax 'string))

(use-package rust-mode
  :if is-personal
  :mode "\\.rs"
  :config
  (eval-when-load
   "lsp-mode"
   (add-hook 'rust-mode-hook 'lsp)))

(use-package cargo
  :after 'rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package typescript-mode
  :if is-personal
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

(use-package geiser
  :if is-personal)

(use-package geiser-guile
  :after geiser
  :config
  ; (add-to-list 'geiser-guile-load-path "~/src/guix")
  (add-to-list 'geiser-guile-load-path "~/src/nonguix"))

(use-package srfi
  :if is-personal
  :config
  (add-hook
   'srfi-mode-hook
   (lambda ()
     (setq-local browse-url-browser-function 'eww))))

(use-package nov
  :disabled
  :if is-personal
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package dumb-jump
  :if is-personal
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eglot :disabled)

;; (use-package tui
;;   :if is-personal
;;   :elpaca
;;    '(:host github :repo "ebpa/tui.el" :files ("*.el" "components" "layout" "demo" "snippets")))

(use-package flycheck)

(use-package circe
  :if is-personal
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
                            (password-store-get host)))))
  (enable-circe-color-nicks)
    
  ;; Don't bombard me with leaves if the leaver hasn't spoke in a while
  (setq circe-reduce-lurker-spam t)

  (defun my-circe-intersect-nicks (buf1 buf2)
    "Does what you think it does. It would make a little sense to remove your own nick from this list, but meh"
    (interactive "b\nb")
    (let ((names1 (with-current-buffer (set-buffer buf1)
                    (circe-channel-nicks)))
          (names2 (with-current-buffer (set-buffer buf2)
                    (circe-channel-nicks))))

      (message (prin1-to-string (-intersection names1 names2))))))

(use-package server
  :elpaca nil
  :config
  (unless (server-running-p) (server-start)))

(use-package emacs
  :elpaca nil
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval progn
           (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph
               (&optional justify)
             (interactive "P")
             (or
              (fill-comment-paragraph justify)
              (let
                  ((paragraph-start
                    (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                   (paragraph-separate
                    (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                   (fill-column
                    (if
                        (and
                         (integerp emacs-lisp-docstring-fill-column)
                         (derived-mode-p 'emacs-lisp-mode))
                        emacs-lisp-docstring-fill-column fill-column)))
                (fill-paragraph justify))
              t))
           (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file default-directory ".dir-locals.el"))))
             (unless
                 (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (geiser-default-implementation quote guix)
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
