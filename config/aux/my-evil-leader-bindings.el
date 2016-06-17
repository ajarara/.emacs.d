(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; todo: organize this a bit better

(evil-leader/set-key "w" `save-buffer)
(evil-leader/set-key "b" `ibuffer)
(evil-leader/set-key "x" `helm-M-x)
(evil-leader/set-key "v" `split-window-right)
(evil-leader/set-key "s" `save-buffer)
;; this is a duplicate with my emacs-leader, but I think I'll keep it.
(evil-leader/set-key "0" `text-scale-adjust)
(evil-leader/set-key "G" `magit-status)
(evil-leader/set-key "f" `helm-for-files)
(evil-leader/set-key "Q" `delete-frame)

(evil-leader/set-key "R" `org-capture)

;; move to init.el. for the future, save a ref to the previous buffer, pressing leader I moves back to it. that would be very useful.
(evil-leader/set-key "I"
  (lambda ()
    (interactive)
    (find-file "~/.emacs.d/init.el")))
