;; -*- lexical-binding: t -*-

(require 'tui)
(require 'tui-hooks)
(require 'tui-use-process-buffer)

(tui-defun-2 tui-process-test-process (&this this)
  "tui-process-test-process"
  (let* ((proc-state (tui-use-process-buffer this '("logger" "-s" "'this will be sent to stderr'"))))
         ;; (proc-state (tui-use-process-buffer this '("ping" "-c" "8" "8.8.8.8"))))
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
                 (funcall elapsed-time-updater #'1+)))))
         (lambda ()
           (cancel-timer timer))))
    (format "%s" elapsed-time)))

(defun tui-process-test ()
  (interactive)
  (tui-render-with-buffer
      (get-buffer-create "*tui-process-test*")
    (tui-process-test-process)))
