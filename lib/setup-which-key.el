;; which-key is fantastic. It provides a visual reminder for any
;; keymap. It even has a dialog for showing the top level binds (I bind
;; it here to C-h SPC)

(use-package which-key
  :straight t
  :demand t
  :diminish which-key-mode
  :bind* 
  (("C-h SPC" . which-key-show-top-level))
  :config
  (which-key-mode))


(provide 'setup-which-key)
