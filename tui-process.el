;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)

(cl-defstruct (tui-process-state (:constructor tui-process-state--create)
                                 (:copier nil))
  process                          ; the process that is ongoing
  stdout-deltas                    ; filter function list aggregation, lifo
  stderr-deltas)                   ; ibid


;; to test stderr use 'logger -s "this-will-be-sent-to-stderr-filter"'
(defun tui-process--execute (command stdout-filter stderr-filter)
  (let* ((stderr-buf (get-buffer-create (symbol-name (gensym " anonymous-stderr-buffer-"))))
         (process (make-process
                   :name (format "tui-process-%s" (string-join command "-"))
                   :command command
                   :stderr stderr-buf
                   :filter stdout-filter
                   :sentinel #'ignore))
         (stderr-process (get-buffer-process stderr-buf)))
    (set-process-filter stderr-process stderr-filter)
    process))

(defun tui-use-process (component command)
  (let* ((deltas-state (tui-use-state component '(nil nil)))
         (just-proc-state (tui-use-state component nil)))
    (tui-use-effect
     component
     (lambda ()
       (let ((maybe-process
              (and
               command
               (tui-process--execute
                command
                (lambda (proc stdout-delta)
                  (let ((deltas-updater (cadr deltas-state)))
                    (funcall
                     deltas-updater
                     (lambda (prev-deltas-state)
                       (list
                        (cons stdout-delta
                              (car prev-deltas-state))
                        (cadr prev-deltas-state))))))
                (lambda (proc stderr-delta)
                  (let ((deltas-updater (cadr deltas-state)))
                    (funcall
                     deltas-updater
                     (lambda (prev-deltas-state)
                       (list
                        (car prev-deltas-state)
                        (cons stderr-delta
                              (cadr prev-deltas-state)))))))))))
         (funcall (cadr just-proc-state)
                  maybe-process)
         (lambda ()
           (if maybe-process
               (kill-process maybe-process)))))
     command)
    (tui-process-state--create
     :process (car just-proc-state)
     :stdout-deltas (caar deltas-state)
     :stderr-deltas (cdar deltas-state))))

(tui-defun-2 tui-process-test-process (&this this)
  "tui-process-test-process"
  (let* ((proc-state (tui-use-process this '("logger" "-s" "'this will be sent to stderr'")))
         (_ (message (tui-process-state-stderr-deltas proc-state)))
         (stderr (string-join (reverse (tui-process-state-stderr-deltas proc-state)) " ")))
    stderr))
         
(tui-defun-2 tui-process-test-component-states (&this this)
  "tui-process-test-component"
  (let ((state (tui-use-state this 15)))
    (cond
     ((eql 15 (car state))
      (message "first render!")
      (funcall (cadr state) 16))
     ((eql 16 (car state))
      (message "second render!")
      (funcall (cadr state) 17)))
    (format "curr-state: %s trailing" (car state))))

(tui-defun-2 tui-process-test-component (&this this)
  "tui-process-test-component"
  (let* ((state (tui-use-state this 15))
         (_ (tui-use-effect this
                            (lambda ()
                              (cond
                               ((eql 15 (car state))
                                (message "first render!")
                                (funcall (cadr state) 16)
                                (message "first render after set-state")
                                ;; why is this being run _after_ second render?
                                (lambda () (message "teardown of first observing %s" (car state))))
                               ((eql 16 (car state))
                                (message "second render!")
                                (funcall (cadr state) 17)
                                (lambda () (message "teardown of second observing %s" (car state))))
                               ((eql 17 (car state))
                                (message "third render!")
                                (lambda () (message "teardown of component!")))))
                            (list (car state)))))
     (format "curr-state: %s trailing" (car state))))

(defun tui-process-test ()
  (interactive)
  (let* ((buffer (get-buffer-create "*tui-process-test*"))
         (component (tui-process-test-process)))
    (tui-render-element
     (tui-buffer
      :buffer buffer
      component))
    (switch-to-buffer buffer))) 

(provide 'tui-process)
