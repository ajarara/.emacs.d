;; -*- lexical-binding: t -*-

(require 'tui-hooks)
(require 'tui-use-rerender)

(defun tui-use-minor-mode (component minor-mode &optional minor-mode-hook)
  "Subscribe to the minor mode. Returned value is whether the mode is enabled."
  (cl-assert (symbolp minor-mode) nil "Quote the minor mode as a symbol")
  (let* ((minor-mode-enabled-wrapped (ignore-errors (list (eval minor-mode-symbol))))
         (_ (cl-assert minor-mode-enabled-wrapped nil
                       "Passed symbol has no value. Is this a minor mode symbol?"))
         (minor-mode-value (car minor-mode-enabled-wrapped))
         (minor-mode-hook (or minor-mode-hook
                              (string-join (list
                                            (symbol-name minor-mode)
                                            "hook")
                                           "-")))
         (rerender-cb (tui-use-rerender component)))
    (tui-use-effect
     component
     (list minor-mode-hook rerender-cb)
     (add-to-hook minor-mode-hook rerender-cb))))

(provide 'tui-use-minor-mode)
                  
     
     
         
         
    

