;; -*- lexical-binding: t -*-

(require 'tui)
(require 'tui-hooks)
(require 'tui-use-process-buffer)

;; (tui-defun-2 tui-process-test-component-states (&this this)
;;   "tui-process-test-component"
;;   (let ((state (tui-use-state this 15)))
;;     (cond
;;      ((eql 15 (car state))
;;       (message "first render!")
;;       (funcall (cadr state) 16))
;;      ((eql 16 (car state))
;;       (message "second render!")
;;       (funcall (cadr state) 17)))
;;     (format "curr-state: %s trailing" (car state))))

;; (tui-defun-2 tui-process-test-component-effects (&this this)
;;   "tui-process-test-component"
;;   (let* ((state (tui-use-state this 15))
;;          (_ (tui-use-effect this
;;                             (car state)
;;                             (lambda ()
;;                               (cond
;;                                ((eql 15 (car state))
;;                                 (message "first render!")
;;                                 (funcall (cadr state) 16)
;;                                 (message "first render after set-state")
;;                                 ;; why is this being run _after_ second render?
;;                                 (lambda () (message "teardown of first observing %s" (car state))))
;;                                ((eql 16 (car state))
;;                                 (message "second render!")
;;                                 (funcall (cadr state) 17)
;;                                 (lambda () (message "teardown of second observing %s" (car state))))
;;                                ((eql 17 (car state))
;;                                 (message "third render!")
;;                                 (lambda () (message "teardown of component!"))))) )))
;;     (format "curr-state: %s trailing" (car state))))

(tui-defun-2 tui-process-test-process (&this this)
  "tui-process-test-process"
  (let* ((proc-state (tui-use-process-buffer this '("logger" "-s" "'this will be sent to stderr'"))))
    (if-let ((proc-state)
             (stderr-buffer (tui-process-buffer-state-stderr-buffer proc-state)))
        (with-current-buffer stderr-buffer
          (buffer-string)))))


(tui-defun-2 tui-process-test-counter (&this this)
  "tui-process-test-counter"
  (let* ((elapsed-time-state (tui-use-state this 0))
         (elapsed-time-updater (cadr elapsed-time-state))
         (elapsed-time (car elapsed-time-state)))
    (tui-use-effect
     this
     elapsed-time-updater
     (lambda ()
       (let ((timer
              (run-at-time
               0
               1
               (lambda ()
                 (funcall elapsed-time-updater
                          (lambda (curr-time) (1+ curr-time)))))))
         (lambda ()
           (cancel-timer timer)))))
    (format "%s" elapsed-time)))


(defun tui-process-test ()
  (interactive)
  (tui-render-with-buffer
      (get-buffer-create "*tui-process-test*")
    (tui-process-test-process)))
