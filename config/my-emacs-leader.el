;; I need a quick interface that allows me to do mostly window manipulations, but also meta operations with emacs. This way I can always rely on these bindings. For the most part. Once I do the below, I can guarantee that this will always work, with the added benefit of seeing what the default key C-, is.
;; TODO: make this a minor mode and enable it globally. This will prevent org mode from overriding
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
;; BEGIN EMACS LEADER BINDINGS

;; yes, I could define this as a pairing and map through. yes, that would be a lot more readable. yes, I don't know why I haven't done it.
;; almost all of these are designed for buffer manipulation. That way when I don't have vim bindings I can always rely on these to do shit.
(my/emacs-leader-bind "C-g" `keyboard-quit)

;; love these
(my/emacs-leader-bind "C-," `other-window)
(my/emacs-leader-bind "o" `delete-other-windows)
(my/emacs-leader-bind "C-o" `delete-other-windows) ; I have no idea why I prefer this, to be honest, I have C-x and C-w as prefixes for the exact same key

;; wish there was a cleaner way to do this so I didn't have to look at this definition, but this is also INVALUABLE
(progn
  (my/emacs-leader-bind "C-c" `my/find-emacs-config-file))

;; what would be interesting is defining a function that interactively got keybinds and asked for an expression and concatenated it to this file.
;; org-capture is significantly better than remember
(my/emacs-leader-bind "C-r" `org-capture)

(my/emacs-leader-bind "C-0" `text-scale-adjust)

;; yes, this is almost wasting a key. I get it.
(my/emacs-leader-bind "C-q" `ido-kill-buffer)

;; toggling visual line mode globally isn't enough. Here's a keybind.
(my/emacs-leader-bind "V" `visual-line-mode)
(my/emacs-leader-bind "v" `split-window-horizontally)

;; quick way to find configuration files
(defun my/find-emacs-config-file ()
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/config/"))
