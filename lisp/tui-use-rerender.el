;; -*- lexical-binding: t -*-

(require 'tui-hooks)


(defun tui-use-rerender (component)
  "Returns a lambda. Calling this lambda with any number of arguments causes the component to queue a render"
  (let* ((trigger-rerender (cadr (tui-use-state component 0))))
    (tui-use-callback
     component
     trigger-rerender
     (lambda (&rest ignored)
       (funcall trigger-rerender '1+)))))


(provide 'tui-use-rerender)
