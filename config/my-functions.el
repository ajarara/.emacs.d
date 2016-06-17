;; after splitting a frame automatically, switch to the new window (unless we
;; were in the minibuffer)
;; found on https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
;; not good for getting rid of help buffers. Maybe a better function would be to just quit-restore-window other-window.
;; window
(setq split-window-preferred-function 'my/split-window-func)
(defun my/split-window-func (&optional window)
  (let ((new-window (split-window-sensibly window)))
    (if (not (active-minibuffer-window))
        (select-window new-window))))
