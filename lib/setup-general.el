;; https://github.com/noctuid/general.el is a package that aims to
;; simplify and unify keybinding. Problems I have with letting
;; bind-key handle it is that it's difficult to tailor. An example: in
;; dired mode, I like having the ability to move by jk or having
;; visual block/line selection. At the same time, I want dired mode
;; bindings available. So I bind "i" to evil-insert-state.  However I
;; have no idea how to do it with bind-key. Further, this allows me to
;; get rid of evil-leader. The less packages I depend on for my
;; output, the better.

(use-package general
  ;; maybe in the future make this config evil agnostic?
  :straight t
  :if (featurep 'evil)
  :config

  ;; leader key binds
  (setq general-default-keymaps '(evil-normal-state-map
                                  evil-visual-state-map
                                  evil-emacs-state-map))

  (setq general-default-prefix "C-c")
  (general-define-key
   
   "g" 'keyboard-quit
   "C-g" 'keyboard-quit
   "SPC" 'ace-window

   "w" 'save-buffer
   "v" 'visual-line-mode
   "t" 'toggle-word-wrap
   "s" 'magit-status
   
   "a" 'counsel-ag
   
   "m" 'fill-region

   "f" 'projectile-find-file
   "c" 'projectile-compile-project
   
   "p" 'my-find-projects
   "o" 'my-find-org-files

   "r" 'org-capture

   "i" 'imenu))


(provide 'setup-general)
