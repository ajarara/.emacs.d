;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'tui-hooks)
(require 'tui-use-hook)
(require 'tui-process)

(cl-defstruct (tui-process-buffer-state (:constructor tui-process-buffer-state--create)
                                        (:copier nil))
  process
  process-status  ; updated after every sentinel call
  stdout-buffer
  stderr-buffer)


(defun tui-use-process-buffer--execute (command stdout-buffer stderr-buffer sentinel)
  (make-process
   :name (format "tui-process-%s" (string-join command "-"))
   :command command
   :buffer stdout-buffer
   :stderr stderr-buffer
   :sentinel sentinel))

(defun tui-use-process-buffer--create-anon-buffer ()
  (get-buffer-create
   (symbol-name (gensym " tui-use-process-buffer--anonymous-buffer-"))))

(defun tui-use-process-buffer--create-anon-symbol ()
  (gensym " anonymous-symbol-"))

(defun tui-use-process-buffer (component command)
  (let* ((rerender-cb (cadr (tui-use-state component nil)))
         (trigger-rerender (tui-use-callback component
                                             rerender-cb
                                             (lambda (&rest _ignored)
                                               (funcall rerender-cb
                                                        (tui-use-process-buffer--create-anon-symbol)))))
         (proc-buffer-state (tui-use-state component nil))
         (proc-buffer-updater (cadr proc-buffer-state)))
    (tui-use-effect
     component
     (list command proc-buffer-updater trigger-rerender)
     (lambda ()
       (let* ((stdout-buffer (tui-use-process-buffer--create-anon-buffer))
              (stderr-buffer (tui-use-process-buffer--create-anon-buffer))
              (sentinel (lambda (proc _)
                          (let ((process-status (process-status proc)))
                            (funcall
                             proc-buffer-updater
                             (lambda (prev-proc-buffer-state)
                               (tui-process-buffer-state--create
                                :process (tui-process-buffer-state-process prev-proc-buffer-state)
                                :process-status process-status
                                :stdout-buffer (tui-process-buffer-state-stdout-buffer prev-proc-buffer-state)
                                :stderr-buffer (tui-process-buffer-state-stderr-buffer prev-proc-buffer-state)))))))
              (_ (progn
                   (with-current-buffer stdout-buffer
                     (push trigger-rerender after-change-functions))
                   (with-current-buffer stderr-buffer
                     (push trigger-rerender after-change-functions))))
              (maybe-process
               (and
                command
                (tui-use-process-buffer--execute command
                                                 stdout-buffer
                                                 stderr-buffer
                                                 sentinel))))
         (funcall proc-buffer-updater
                  (tui-process-buffer-state--create
                   :process maybe-process
                   :process-status (and maybe-process (process-status maybe-process))
                   :stdout-buffer stdout-buffer
                   :stderr-buffer stderr-buffer))
         (lambda ()
           (if maybe-process
               (condition-case nil
                   (kill-process maybe-process)
                 (error nil)))
           (kill-buffer stdout-buffer)
           (kill-buffer stderr-buffer)))))
    (car proc-buffer-state)))
           
           

(provide 'tui-use-process-buffer)


