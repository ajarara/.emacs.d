;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)

(defun tui-use-hook (component hook)
  "Rerender component whenever hook is invoked."
  (let* ((trigger-render (cadr (tui-use-state component nil))))
    (tui-use-effect
     component
     trigger-render
     (lambda ()
       (add-hook hook trigger-render)
       (lambda ()
         (remove-hook hook trigger-render))))))

(defun tui-use-hook-abnormal (component abnormal-hook callback)
  "Register callback against hook using an effect."
  (tui-use-effect component
                  callback
                  (lambda ()
                    (push callback abnormal-hook)
                    (lambda ()
                      (delete callback abnormal-hook)))))

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
      (tui-use-hook-abnormal component after-change-functions #'trigger-rerender))))

(provide 'tui-use-hook)
