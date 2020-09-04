;; My relationship with evil is a healthy one. I can navigate fine
;; without the keybinds but a lot of the constructs are already
;; ingrained, and bound for me conveniently. Take #. The emacs
;; alternative is word-search-backward, but I would have to bind it
;; myself. I'd also have to write advice to provide the default word at
;; point for equivalent functionality. Why do I have to ditch #? We have
;; a good thing going. Let's keep it that way.

(use-package evil
  :straight t
  :init
  ;; evil overrides my C-z key with evil-toggle-state - but if I set
  ;; this early enough, it no longer gets overridden
  (setq evil-toggle-key "C-`")
  
  ;; evil's undo is a little strong, especially since I'm staying in
  ;; insert ("emacs", later explained) mode more often.
  (setq evil-want-fine-undo t)

  :config
  ;; Turn evil mode on globally
  (evil-mode t)

  ;; the below is used to have emacs be the default state, but allow
  ;; me to drop in (pop up?) to evil if need be.  more config is
  ;; available in the URL contained within the progn

  ;; there are significantly less context switches now. when I am in a
  ;; new buffer, I know I am in emacs state, and can begin typing
  ;; immediately. If I want to do anything evil, just hit esc to elevate
  ;; to normal state.

  ;; http://stackoverflow.com/a/27794225/2932728
  (defalias 'evil-insert-state 'evil-emacs-state) 

  (setq evil-default-state 'emacs)

  ;; I should move this to general config, but there's no guarantee if
  ;; that's defined yet unless I put it in bootstrap. It would make
  ;; sense to if I want to unify all keybinds in the same mechanism.
  ;; Maybe bootstrap is the wrong name, and instead something like preamble?
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)


  ;; a quick way to differentiate which state I'm in without looking
  ;; at the mode line
  (setq evil-emacs-state-cursor `(hbar . 2))

  :demand t
  ;; Needs to also be sent to general.
  :bind* (:map evil-emacs-state-map
               ("C-r" . evil-paste-from-register)

               :map evil-normal-state-map
               ("C-f" . evil-scroll-down)
               ("C-b" . evil-scroll-up)
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("'" . evil-goto-mark)
               ("C-e" . end-of-line)
               ("C-y" . yank)
               ("C-d" . evil-scroll-down)
               ("C-t" . ace-window)

               :map evil-motion-state-map
               ("C-f" . evil-scroll-down)
               ("C-b" . evil-scroll-up)))

;; inserts highly visible characters at marks. they go away upon
;; entering visual mode or 'insert' mode.
(use-package evil-visual-mark-mode
  :straight t
  :config
  (evil-visual-mark-mode))

(provide 'setup-evil)
