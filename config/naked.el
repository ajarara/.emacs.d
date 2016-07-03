;; This configuration file will work with stock emacs, and configures mostly appearance, and some minor fixes.

;; this is needed, otherwise emacsclient hangs upon exit when it has something in the kill ring
(setq x-select-enable-clipboard-manager nil)

;; removing all the visual goodies
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; add in a useful visual goodie
(visual-line-mode 1)
;; why is this the only way to access word wrap.. -_-
(toggle-word-wrap)


;; decent enough theme
;;(load-theme 'misterioso t)

;; let's see what this does.
;; another example of stupid variable interfaces. if I'm setting this, I'd want to have debugging on all the time
;; but I have to unset this when I explicitly enable init-debugging or something.
;;(toggle-debug-on-error)



;; open files ending in .scm, .rkt into lisp-mode
(add-to-list `auto-mode-alist `("\\.rkt\\.scm" . scheme))

;; FROM http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
;; store all backup and autosave files in the tmp dir
;; still have to deal with garbage files, though. it's really god damn annoying to have those be added in git
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ido improvements (disabled because ivy is just plain better)
;;(setq ido-enable-flex-matching t)
;;(setq ido-ignore-extensions t)

(setq split-height-threshold nil)
;; tried 150 and did not get desired effect
(setq split-width-threshold 140)

;; please enter yes/no, no screw that, I just want y/n
(fset `yes-or-no-p `y-or-n-p)

;; echo keystrokes quicker, helps a lot with prefix keys.
;; heavily recommend which-key to the emacs newbie
(setq echo-keystrokes 0.1)

;; paste at point, not on location of click (thanks wasamasa!)
(setq mouse-yank-at-point t)

;; ignore dialog boxes
(setq use-dialog-box nil)
;; BINDINGS

;; changed from M-e to M-o, also, M-o does not work with describe key.
;; M-o evaluates the whole sexp at point
(global-set-key (kbd "M-o") `eval-defun)

;; use ibuffer for heavy lifting
(global-set-key (kbd "C-x C-b") 'ibuffer)
