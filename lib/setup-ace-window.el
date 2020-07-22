;; C-x o is way too much to switch between windows (which is something
;; I find myself doing constantly).

(use-package ace-window
  :straight t
  :bind*
  (("C-t" . ace-window))
  :config
  (setq aw-scope 'frame))


(provide 'setup-ace-window)
