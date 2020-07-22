;; Some of these config options were brought to me by wasamasa's config:
;; https://github.com/wasamasa/dotemacs/blob/master/init.org

;; any dialog that asks for confirmation requires that you type in the
;; full yes or no (and ignores whatever is after it, but does not
;; accept y/n). Setting it to y-or-n-p allows you to do this. And you
;; don't have to hit RET.
(fset `yes-or-no-p `y-or-n-p)

;; Echoing keystrokes allows you to see exactly what you've typed
;; almost immediately. Doesn't sound useful, but it is appreciated
;; often.
(setq echo-keystrokes 0.1)

;; Tell emacs to paste at point, not on location of click. Now if C-y
;; doesn't work, I can use my mouse without caring about precision.
(setq mouse-yank-at-point t)


;; Don't ask me to follow symlinks, just warn me and do it always.
(setq vc-follow-symlinks nil)


;; Emacs should let me be as stupid as I want.
(setq disabled-command-function nil)

(provide 'setup-snappy)
