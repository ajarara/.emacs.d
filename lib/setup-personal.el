;; this file is a mess -- lots of keybinds.
;; simple scrolling
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; remove backup clutter.
(make-directory "~/.emacs.d/backup" t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

(make-directory "~/.emacs.d/autosave" t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Prefer horizontal splits when the frame has the space for it.
;; By horizontal, I mean vim's and the rest of the world's notion of vertical.

;; You split along the horizontal axis, I guess. Sure.
(setq split-height-threshold nil)
;; This works on a small 11 in screen, but I have a big screen with a docking station at home.
;; It would be great to preserve this behavior by relative size of screen, not absolute.
(setq split-width-threshold 140)

(setq-default indent-tabs-mode nil)

(defun my-smarter-move-beginning-of-line (arg)
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
                'my-smarter-move-beginning-of-line)

(defun my-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun my-github (query)
  (interactive "sSearch Github: ")
  (browse-url (format "https://github.com/search?q=%s" query)))

;; non obtrusive version of helm-google-suggest. Although helm-google-suggest is more fun
(defun my-google (query)
  "It's mine! MIIIIIIINE!"
  (interactive "sSearch the googs: ")
  (browse-url (format "https://google.com/#q=%s" query)))

;; should migrate to general
(bind-key* "C-h" `help-command)
(bind-key* "C-h C-h" (lambda ()
    (interactive) (info "(emacs) Help Summary")))

(bind-key* "M-[" `previous-buffer)
(bind-key* "M-]" `next-buffer)

(bind-key* "M-." `xref-find-definitions-other-window)

;; because Esc turns into meta.
(define-key key-translation-map (kbd "C-M-g") (kbd "C-g"))

(global-set-key (kbd "M-/") 'hippie-expand)

(bind-key* "M-q" `quoted-insert)

;; shadows universal arg, I think? Damn, I need to read the manual.
(bind-key* "C-0" `text-scale-adjust)

;; shadows capitalize word (used to be my minor mode keymap, I moved all that to evil-leader, which I may eventually move to general)
(bind-key "M-c" `comment-dwim)

;; shadows indent-new-comment-line
(bind-key* "M-j" `end-of-buffer)

;; shadows move-to-window-line-top-bottom
(bind-key* "M-r" `delete-other-windows)

;; would like this instead to just kill the buffer, or like rotate. I think I need some buffer management tool
;; shadows kill-ring-save
(bind-key* "M-w" `delete-window)

;; I don't actually know what the name of the function is, but I know I don't need it. It's some typeface stuff.
;; also, the function name here is misleading, it evaluates the whole top-level expression, from anywhere in the expression, not just defuns
;; shadows Set face:

;; I'm gonna need shackle just for this async.
;; shadows universal argument, 7
(bind-key* "M-7" `async-shell-command)

;; shadows universal argument, 1
(bind-key* "M-1" `shell-command)

;; shadows prefix containing occur
(bind-key* "M-s" 'switch-to-buffer)

;; shadows tab-to-tab-stop
(bind-key* "M-i" `my-find-init-file)

;; instantly kills buffer (without deleting the window), unless unsaved content. this advices kill-buffer
;; shadows kill-sentence
(bind-key* "M-z" `kill-this-buffer)

;; U for undeaaaaaaaaaaaaaaaaad
;; shadows upcase-word
(bind-key* "M-u" `bury-buffer)

;; shadows nothing that I know of.
;; (bind-key* "M-p" `my-find-projects)

;; this leaves M-d free, for something. Although I use mode-d for colon/semicolon
;; shadows kill-sentence
(bind-key* "M-k" `kill-word)

;; shadows nothing
(bind-key* "M-\"" `insert-pair)

;; shadows nothing
(bind-key* "<f5>" `recompile)

(provide 'setup-personal)
