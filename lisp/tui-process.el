;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)


(cl-defstruct (tui-process-state (:constructor tui-process-state--create)
                                 (:copier nil))
  process
  process-status                   ; updated after every sentinel call
  stdout-deltas                    ; filter function list aggregation, lifo
  stderr-deltas)                   ; ibid

(defun tui-process-state-is-done (tui-process-state)
  (let ((proc-status (tui-process-state-process-status tui-process-state))
        (done-states '(exit signal)))
    (memql proc-status done-states)))

(cl-defun tui-process--execute (&key command stdout-filter stderr-filter sentinel)
  (let* ((stderr-buf (get-buffer-create (symbol-name (gensym " anonymous-stderr-buffer-"))))
         (process (make-process
                   :name (format "tui-process-%s" (string-join command "-"))
                   :command command
                   :stderr stderr-buf
                   :filter stdout-filter
                   :sentinel sentinel))
         (stderr-process (get-buffer-process stderr-buf)))
    (set-process-filter stderr-process stderr-filter)
    process))

(defun tui-use-process (component command)
  (let* ((proc-status-state (tui-use-state component nil))
         (stdout-state (tui-use-state component '()))
         (stderr-state (tui-use-state component '()))
         (just-proc-state (tui-use-state component nil)))
    (tui-use-effect
     component
     command
     (lambda ()
       (let ((maybe-process
              (and
               command
               (tui-process--execute
                :command command
                :stdout-filter (lambda (_ stdout-delta)
                                 (let ((stdout-updater (cadr stdout-state)))
                                   (funcall
                                    stdout-updater
                                    (lambda (prev-stdout)
                                      (cons stdout-delta
                                            prev-stdout)))))
                :stderr-filter (lambda (_ stderr-delta)
                                 (let ((stderr-updater (cadr stderr-state)))
                                   (funcall
                                    stderr-updater
                                    (lambda (prev-stderr)
                                      (cons
                                       stderr-delta
                                       prev-stderr)))))
                :sentinel (lambda (proc _)
                            (let ((proc-status-state-updater (cadr proc-status-state))
                                  (proc-status (process-status proc)))
                              (funcall proc-status-state-updater proc-status)))))))
         (funcall (cadr just-proc-state)
                  maybe-process)
         (lambda ()
           (if maybe-process
               ;; might be dead already
               (condition-case nil
                   (kill-process maybe-process)
                 (error nil)))))))
    (tui-process-state--create
     :process (car just-proc-state)
     :process-status (car proc-status-state)
     :stdout-deltas (car stdout-state)
     :stderr-deltas (car stderr-state))))

(provide 'tui-process)
