;; after splitting a frame automatically, switch to the new window (unless we
;; were in the minibuffer)
;; found on https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
;; not good for getting rid of help buffers. Maybe a better function would be to just quit-restore-window other-window.
;; window
;;(setq split-window-preferred-function 'my/split-window-func)
;;(defun my/split-window-func (&optional window)
;;  (let ((new-window (split-window-sensibly window)))
;;    (if (not (active-minibuffer-window))
;;        (select-window new-window))))
;; DISABLED because I now use ace-window to manage my buffers, and would rather the point move consistently.


;; should I move this func to the my namespace?
;; meh.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; I type it enough for it to be useful.
;; maybe a cool package idea would be to suggest a more interactive way to define keybinds
;; based on frequency of M-x invocations
;; another would be a visualization of all keybinds of all minor/major modes installed, so you
;; can find a key that you'll be fine with 
(global-set-key (kbd "C-h SPC") `which-key-show-top-level)

;; C-x 0 is too much for an operation that I do all the damn time.
(defun my/znc ()
  (interactive)
  (erc-tls
   :server "jarmac.org"
   :port 6697))
