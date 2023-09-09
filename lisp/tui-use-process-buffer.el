;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks-x)

(cl-defstruct (tui-process-buffer-state (:constructor tui-process-buffer-state--create)
                                        (:copier nil))
  process
  stdout-buffer
  stderr-buffer)

(defun tui-use-process-buffer--anon-buffer ()
  (get-buffer-create
   (symbol-name
    (gensym " tui-use-process-buffer--anon-buffer-"))))

(defun tui-use-rerender (component)
  (let* ((arbitrary-state (tui-use-state component nil))
         (set-arbitrary-state (cadr arbitrary-state)))
    (tui-use-callback
     component
     (list set-arbitrary-state)
     (lambda (&rest _ignored)
       (funcall set-arbitrary-state (gensym "tui-use-process-arbitrary"))))))

;; create a pipe process, use that as stderr
(defun tui-use-process-buffer (component command)
  (let* ((proc-state (tui-use-state component nil))
         (set-proc-state (cadr proc-state))
         (trigger-rerender (tui-use-rerender component)))
    (tui-use-effect
     component
     (list command set-proc-state trigger-rerender)
     (lambda ()
       (when command
         (let* ((stderr-buffer (tui-use-process-buffer--anon-buffer))
                (stdout-buffer (tui-use-process-buffer--anon-buffer))
                ;; we create a pipe process so we can install
                ;; a filter before the upstream starts
                (process
                 (make-process
                  :name (format "tui-process-%s" (string-join command "-"))
                  :command command
                  :buffer stdout-buffer
                  :stderr stderr-buffer
                  :noquery t
                  :filter (lambda (_ stdout-delta)
                            (with-current-buffer stdout-buffer
                              (insert stdout-delta))
                            (funcall trigger-rerender))
                  :sentinel (lambda (&rest ignored) (funcall trigger-rerender)))))
           (funcall set-proc-state
                    (tui-process-buffer-state--create
                     :process process
                     :stdout-buffer stdout-buffer
                     :stderr-buffer stderr-buffer))
           (lambda ()
             ;; could already be complete
             (ignore-errors
               (kill-process process))
             ;; not sure if this is necessary
             (kill-buffer stderr-buffer)
             (kill-buffer stdout-buffer))))))
    (car proc-state)))

(provide 'tui-use-process-buffer)


