;; Dired is deep. Just.. don't play around with my keymaps.

(use-package dired
  :after (general)
  :config
  (define-key dired-mode-map (kbd "SPC") nil)
  (define-key dired-mode-map (kbd "M-s") nil)
  
  ;; remove dired-mode-map definition
  (define-key dired-mode-map (kbd "i") nil)
  
  (general-define-key :prefix nil
                      :keymaps 'dired-mode-map
                      :states '(normal)
                      "i" 'evil-insert-state)
  
  (general-define-key :prefix nil
                      :keymaps 'dired-mode-map
                      :states '(emacs)
                      "i" 'dired-maybe-insert-subdir))


(provide 'setup-dired)
