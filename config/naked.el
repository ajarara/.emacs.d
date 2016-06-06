;; This configuration file will work with stock emacs, and configures mostly appearance, and some minor fixes.
;; this is needed, otherwise emacsclient hangs upon exit when it has something in the kill ring
(setq x-select-enable-clipboard-manager nil)

;; ido mode an objective improvement over switch-to-buffer. since i use helm's find file, only apply to the buffer menu.
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

;; BINDINGS

;; M-e evaluates the whole sexp at point
(global-set-key (kbd "M-e") `eval-defun)
