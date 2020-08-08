;; casual inspection of DOMS and JS objects on the fly

(use-package indium
  :disabled t
  :config
  ;; delete all jsm modes..
  ;; I wonder what disqualifies a mode from being applicable to the environment.
  ;; (setq auto-mode-alist (assq-delete-all "\\.jsm?\\'" auto-mode-alist))
  ;; make js2-mode (javascript-IDE) the default
  ;; (setq auto-mode-alist (add-to-list '("\\.jsm?\\'" . js2-mode) auto-mode-alist))
  (add-hook 'js2-mode-hook 'indium-interaction-mode))
(setq indium-chrome-executable "chromium-browser")

(provide 'setup-indium)
