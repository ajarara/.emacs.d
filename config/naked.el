;; This configuration file will work with stock emacs, and configures mostly appearance, and some minor fixes.
;; this is needed, otherwise emacsclient hangs upon exit when it has something in the kill ring
(setq x-select-enable-clipboard-manager nil)

;; welcome back IDO mode!
(ido-mode `buffers)

;; use ibuffer for heavy lifting
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; removing all the visual goodies
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; add in a useful visual goodie
(visual-line-mode 1)
;; decent enough theme
(load-theme 'misterioso t)


;; open files ending in .scm, .rkt into lisp-mode
(add-to-list `auto-mode-alist `("\\.rkt\\.scm" . scheme))

;; FROM http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; slower, saner scrolling
(setq scroll-step 1
         scroll-conservatively 10000)

;; echo keystrokes quicker, helps a lot with prefix keys.
;; heavily recommend which-key to the emacs newbie
(setq echo-keystrokes 0.1)
;; BINDINGS

;; M-e evaluates the whole sexp at point
(global-set-key (kbd "M-e") `eval-defun)

;; C-tab switches windows
;; I find myself using the window ops a LOT, I can't tag it to a 2 double chorded thing.
(global-set-key (kbd "C-<tab>") `other-window)
