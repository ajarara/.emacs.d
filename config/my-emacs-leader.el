(define-prefix-command `my/emacs-leader-keymap)

;; just represent your leader as a string
(defvar my/emacs-leader "C-,")
(global-set-key (kbd my/emacs-leader) `my/emacs-leader-keymap)

;; now just use this function to bind things
(defun my/emacs-leader-bind (key-as-string quoted-command)
    (global-set-key (kbd (concat my/emacs-leader
				 " "
				 key-as-string))
		    quoted-command))

(my/emacs-leader-bind "C-," `other-window)

(my/emacs-leader-bind "C-o" `delete-other-windows)
(my/emacs-leader-bind "o" `delete-other-windows) ; I have no idea why I prefer this, to be honest, I have C-x and C-w as prefixes for the exact same key
(defun my/find-emacs-config-file ()
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/config/"))

(my/emacs-leader-bind "C-c" `my/find-emacs-config-file) 

;; what would be interesting is defining a function that interactively got keybinds and asked for an expression and concatenated it to this file.

(my/emacs-leader-bind "C-r" `org-capture)
