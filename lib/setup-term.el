;; I'm not exactly happy with term. There's a bunch of workarounds and
;; keys get rebound and still I encounter usability issues. But it
;; works enough, and I have good clipboard (and visual selection)
;; interaction. So it stays. Maybe someone with knowledge of how to do
;; things the right way will get a real terminal working by embedding
;; or linking to the binary.

;; I use term instead of ansi-term (tried it in a previous iteration)
;; because it's much clearer to configure. I still have no idea how to
;; make ansi-term not insert line breaks.

(use-package term 
  :config
  ;; most of this config is from:
  ;; http://echosa.github.io/blog/2012/06/06/improving-ansi-term/

  ;; don't modify my output please (note this breaks when displaying
  ;; multiline commands at the bottom of the buffer)
  (setq term-suppress-hard-newline t)

  ;; kill the buffer after finishing.
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  ;; why is this not the default? 
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)

  ;; eh.. this makes me sad. All I wanted was C-x.
  ;; (defun my-ad-term-line-mode (_arg)
  ;;   (term-line-mode))
  ;; (advice-add 'term :after #'my-ad-term-line-mode)
  ;; (advice-add 'ansi-term :after #'my-ad-term-line-mode)
  

  ;; 2048 lines of output is way too restrictive.
  (setq term-buffer-maximum-size 8192))

(provide 'setup-term)
