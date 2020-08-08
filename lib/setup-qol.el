;; Prefer horizontal splits when the frame has the space for it.  By
;; horizontal I mean vim's (and the rest of the world's) notion of
;; vertical. You split along the horizontal axis, I guess. Sure.
(setq split-height-threshold nil)
;; This works on a small 11 in screen, but I have a big screen with a docking station at home.
;; It would be great to preserve this behavior by relative size of screen, not absolute.
(setq split-width-threshold 140)

(setq-default indent-tabs-mode nil)

(provide 'setup-qol)
