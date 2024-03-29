;; -*- lexical-binding: t -*-

(require 'tui)
(require 'tui-hooks)
(require 'tui-use-process-buffer)

(tui-defun-2 tui-process-test-stdout-process (&this this)
  "tui-process-test-process"
  (let* ((proc-state (tui-use-process-buffer this '("logger" "-s" "'this will be sent to stderr'"))))
    (tui-process-component :process-buffer-state proc-state)))

(tui-defun-2 tui-process-test-stderr-process (&this this)
  "tui-process-test-process"
  (let* ((process-buffer-state (tui-use-process-buffer this '("ping" "-c" "2" "8.8.8.8"))))
    (tui-process-component :process-buffer-state process-buffer-state)))

(tui-defun-2 tui-process-test-composed-process (&this this)
  "tui-process-test-process"
  (let* ((process-buffer-state (tui-use-process-buffer this '("sh" "-c" "sleep 5; ping -c 8 8.8.8.8; sleep 5"))))
    (tui-process-component :process-buffer-state process-buffer-state)))

;; doesn't use a process lol
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
    (format "%s" elapsed-time))))

(defun tui-process-test ()
  (interactive)
  (tui-render-with-buffer
      (get-buffer-create "*tui-process-test*")
    (tui-process-test-composed-process)))
