;; -*- lexical-binding: t -*-
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(defvar my-lisp (expand-file-name "lisp/" user-emacs-directory)
  "Added to load path. Also holds custom.el")

(add-to-list 'load-path my-lisp)
(setq custom-file (expand-file-name "custom.el" my-lisp))

(require 'cl-lib)
(require 'json)

;; global custom minor modes
(defvar attributes
  '(is-personal has-guix has-org has-magit has-self)
  "Attributes are minor modes that are enabled on a per host basis")
(cl-loop for attribute in attributes
         do (eval `(define-minor-mode ,attribute nil :global t)))

(defmacro subscribe-to-attribute (attribute fn-def)
  (declare (indent defun))
  (cl-assert (memq attribute attributes))
  (let ((hook-sym (intern (concat (symbol-name attribute) "-hook"))))
    `(progn
       (add-hook ',hook-sym ,fn-def)
       (funcall ,fn-def))))


(require 'profile nil t)
(if (boundp 'profile)
    (cl-loop for attr in profile
             do (unless (memq attr attributes)
                  (error "unrecognized attribute from profile: %s" attr))
             do (funcall attr))
  (warn "profile not available"))

(subscribe-to-attribute has-self
  (defun has-self-warning ()
    (and
     has-self
     (not (file-exists-p "~/self"))
     (display-warning :error "Clone the self repository!!"))))


(defmacro use-package-conditionally (name mode &rest body)
  (declare (indent defun))
  `(subscribe-to-attribute ,mode
     (when ,mode
       (use-package ,name ,@body))))

(defmacro replace-or-add (place value selector)
  "Replace first element SELECTOR returns true for in PLACE. 
If no element found, add VALUE to the end of the list."
  `(setf ,place
         (named-let recur ((ls ,place))
           (cond
            ((null ls)
             (list ,value))
            ((funcall ,selector (car ls))
             (cons ,value (cdr ls)))
            (t (cons (car ls)
                     (recur (cdr ls))))))))

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
(use-package markdown-mode)
(use-package company)
(use-package git-link)

(use-package-conditionally magit has-magit)

(use-package ansi-color
  :config
  (add-hook
   'compilation-filter-hook
   (defun ansi-color-my-compilation-hook ()
     (let ((buffer-read-only nil))
       (ansi-color-apply-on-region compilation-filter-start (point))))))

(use-package desktop-environment
  :config
  (let ((screenies (expand-file-name "~/screenies")))
    (unless (file-exists-p screenies)
      (mkdir screenies))
    (setq desktop-environment-screenshot-directory screenies)))

(use-package winner
  :after general
  :config
  (general-define-key
   "M-I" 'winner-undo
   "M-O" 'winner-redo)
  (winner-mode))

(use-package savehist
  :config
  (savehist-mode 1))

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
  :config
  (general-define-key "C-s" 'consult-line)
  (general-define-key "C-c s" 'consult-line-multi)
  (general-define-key "C-h a" 'consult-apropos))

(use-package ghostel)

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

(use-package treesit-auto
  :config
  (add-to-list 'treesit-language-source-alist
             '(starlark "https://github.com/tree-sitter-grammars/tree-sitter-starlark"))
  (global-treesit-auto-mode))

(use-package eglot)

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
  :config
  ;; (add-to-list 'geiser-guile-load-path "~/upstream/nonguix")
  (subscribe-to-attribute has-self
    (defun has-self-add-guile-load-path ()
      (if has-self
          (setq geiser-guile-load-path (cl-pushnew "~/self" geiser-guile-load-path))
        (setq geiser-guile-load-path (cl-delete "~/self" geiser-guile-load-path)))))
  (subscribe-to-attribute has-guix
    (defun has-guix-use-guix-repl ()
      (if has-guix
          (setq geiser-guile-binary (list "guix" "repl"))
        (setq geiser-guile-binary "guile")))))

(use-package paredit
  :config
  (with-eval-after-load 'scheme
    (add-hook 'scheme-mode-hook 'enable-paredit-mode))
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package-conditionally srfi is-personal
  :config
  (add-hook
   'srfi-mode-hook
   (lambda ()
     (setq-local browse-url-browser-function 'eww))))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package-conditionally subed is-personal
  :straight '(:type git :host github :repo "sachac/subed" :files ("subed/*.el"))
  :config
  ;; Remember cursor position between sessions
  (add-hook 'subed-mode-hook 'save-place-local-mode)
  ;; Break lines automatically while typing
  (add-hook 'subed-mode-hook 'turn-on-auto-fill)
  ;; Break lines at 40 characters
  (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40)))
  ;; Some reasonable defaults
  (add-hook 'subed-mode-hook 'subed-enable-pause-while-typing)
  ;; As the player moves, update the point to show the current subtitle
  (add-hook 'subed-mode-hook 'subed-enable-sync-point-to-player)
  ;; As your point moves in Emacs, update the player to start at the current subtitle
  (add-hook 'subed-mode-hook 'subed-enable-sync-player-to-point)
  ;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
  (add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
  ;; Loop over subtitles
  (add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
  ;; Show characters per second
  (add-hook 'subed-mode-hook 'subed-enable-show-cps))

(use-package-conditionally circe has-self
  :requires auth-source
  :config
  (setq circe-network-defaults nil)
  (setq lui-fill-column 63)
  ;; on disconnect, just stay disconnected.
  (setq circe-lagmon-reconnect-interval nil)
  
  (setq circe-network-options
        `(("libera"
           :tls t
           :host "irc.libera.chat"
           :port 6697
           :sasl-strict t
           :sasl-username "ajarara"
)))
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

(use-package-conditionally gptel has-self
  :config
  (setq
   gptel-model   'test
   gptel-backend (gptel-make-openai "llama-cpp"
                   :stream t
                   :protocol "http"
                   :host "localhost:8080"
                   :models '(test))))

(use-package-conditionally nov is-personal
  :config
  (add-to-list 'auto-mode-alist '("\\.epub" . nov-mode)))

(use-package detached
  :init
  (detached-init)
  (setq detached-notification-function 'ignore)
  (setq detached-shell-command-initial-input nil)
  :bind
  (([remap async-shell-command] . detached-shell-command)
   ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

(use-package-conditionally sops-file is-personal
  :straight (:host github :repo "ajarara/sops-file.el")
  :config
  (sops-file-auto-mode 1)
  (setq sops-file-disable-pinentry t)
  (setq sops-file-skip-unavailable-smartcards nil)
  (setq sops-file-auto-mode-regex "\\.enc\\."))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

(use-package emacs
  :after general
  :ensure nil
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
  (setq source-directory "~/upstream/emacs/src")

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

  ;; distinct from adb, this is an android development environment managed by flatpak's Android Studio
  (replace-or-add 'tramp-methods
           `("android"
             (tramp-login-program "flatpak")
             (tramp-login-args (("run") ("--command=/bin/bash") ("com.google.AndroidStudio")))
             (tramp-remote-shell ,tramp-default-remote-shell))
           (lambda (method)
             (equal (car method) "android")))

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

(add-hook 'elpaca-after-init-hook
          (defun load-in-custom-file ()
            (load custom-file 'noerror)))
