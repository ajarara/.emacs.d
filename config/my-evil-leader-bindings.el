(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key "w" `save-buffer)
(evil-leader/set-key "b" `ibuffer)
(evil-leader/set-key "x" `helm-M-x)
(evil-leader/set-key "," 'other-window)
(evil-leader/set-key "v" 'split-window-right)
