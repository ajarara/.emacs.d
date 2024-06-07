;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)
(require 'tui-hooks-x)


(defun tui-use-hook (component hook callback)
  "Register callback against hook using an effect."
  (tui-use-effect component
                  (list hook callback)
                  (lambda ()
                    (add-hook hook callback)
                    (lambda ()
                      (remove-hook hook callback)))))

(defun tui-use-hook--create-anon-symbol ()
  (gensym " anonymous-symbol-"))

(defun tui-use-hook-buffer-change (component buf)
  (let* ((rerender-cb (cadr (tui-use-state component nil)))
         (trigger-rerender (tui-use-callback component
                                             rerender-cb
                                             (lambda (&rest _ignored)
                                               (funcall rerender-cb
                                                        (tui-use-process-buffer--create-anon-symbol))))))
    (with-current-buffer buf
      (tui-use-hook component after-change-functions #'trigger-rerender))))

(provide 'tui-use-hook)
