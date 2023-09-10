;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)

(cl-defstruct (tui-process-buffer-state (:constructor tui-process-buffer-state--create)
                                        (:copier nil))
  process
  stdout-buffer
  stderr-buffer)

(defun tui-use-process-buffer--anon-buffer ()
  (get-buffer-create
   (symbol-name
    (gensym " tui-use-process-buffer--anon-buffer-"))))

(defun tui-use-process-buffer--process-filter (buffer-to-write trigger-rerender)
  (lambda (_ delta)
    (with-current-buffer buffer-to-write
      (insert delta))
    (funcall trigger-rerender)))

(defun tui-use-rerender (component)
  (let* ((arbitrary-state (tui-use-state component nil))
         (set-arbitrary-state (cadr arbitrary-state)))
    (tui-use-callback
     component
     (list set-arbitrary-state)
     (lambda (&rest _ignored)
       (funcall set-arbitrary-state (gensym "tui-use-process-arbitrary"))))))

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
                (stderr-pipe-process
                 (make-pipe-process
                  :name (format "tui-process-stderr-%s" (string-join command "-"))
                  :buffer stderr-buffer
                  :noquery t
                  :filter (tui-use-process-buffer--process-filter
                           stderr-buffer
                           trigger-rerender)
                  ;; suppress default sentinel writing to buffer
                  :sentinel #'ignore))
                (process
                 (make-process
                  :name (format "tui-process-%s" (string-join command "-"))
                  :command command
                  :buffer stdout-buffer
                  :stderr stderr-pipe-process
                  :noquery t
                  :filter (tui-use-process-buffer--process-filter
                           stdout-buffer
                           trigger-rerender)
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
             (ignore-errors
               (kill-process stderr-pipe-process))
             (kill-buffer stderr-buffer)
             (kill-buffer stdout-buffer))))))
    (car proc-state)))

(provide 'tui-use-process-buffer)
