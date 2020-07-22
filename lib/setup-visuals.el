;; can be recovered with <F10>
(tool-bar-mode -1)

;; not needed, just look at the mode line for gauging where you are in the buffer
(scroll-bar-mode -1)

;; wait which one is which
(menu-bar-mode -1)

;; you can set this to some arbitrary number so that it will blink for that number of times. I don't like blinking.
(blink-cursor-mode 0)

;; pretty quotes can't be jumped to easily.
(setq text-quoting-style 'grave)

;; Might be considered a hindrance, but I rarely need more than 80
;; columns while programming and would rather not have to deal with
;; horizontal scrolling. These two settings are neat for prose, and
;; org mode.
(visual-line-mode 1)


;; Initially I just used the package (named simple-scrolling), but
;; found that it caused weird graphical issues with browsing manual
;; pages. This works just fine, although sometimes I miss the ability
;; to have margins, but I also don't miss the jumpiness that sometimes
;; occurred.
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; I don't know how I feel using hl-line-mode on EVERY
;; buffer. Instead, let's just add the ones that make sense.
(defvar my-hl-line-mode-hook-list
  `(prog-mode-hook
    circe-mode-hook))

(dolist (this-mode-hook my-hl-line-mode-hook-list)
  (add-hook this-mode-hook `hl-line-mode))


(defun setup-disable-visuals-unload-function ()
  (tool-bar-mode 1)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (blink-cursor-mode 10)
  (setq text-quoting-style nil)
  (visual-line-mode -1)
  (setq scroll-conservatively 0)
  (setq auto-window-vscroll t)
  (dolist (this-mode-hook my-hl-line-mode-hook-list)
    (remove-hook this-mode-hook `hl-line-mode)))

(provide 'setup-visuals)
