;; -*- lexical-binding: t -*-

(require 'tui)
(require 'tui-components)
(require 'cl-lib)
(require 'tui-hooks)

(cl-defstruct (tui-process-buffer-state (:constructor tui-process-buffer-state--create)
                                        (:copier tui-process-buffer-state--copy))
  process
  stdout-buffer
  stderr-buffer
  ;; a counter so that filter functions may trigger
  ;; rerenders even when this struct is passed as a prop
  -internal)

(defun tui-process-buffer-is-done (process-buffer-state)
  (and process-buffer-state
       (memq (process-status
              (tui-process-buffer-state-process process-buffer-state))
             '(exit signal))))
    
(defun tui-use-process-buffer--anon-buffer ()
  (get-buffer-create
   (symbol-name
    (gensym " tui-use-process-buffer--anon-buffer-"))))

(defun tui-use-process-buffer--signal-update (proc-state)
  ;; we increment -internal so that we issue rerenders
  ;; to all components that depend on it
  ;; we use this in two places:
  ;; - process filter functions
  ;; - process sentinels
  (and proc-state
       (let ((new-proc-state (tui-process-buffer-state--copy proc-state)))
         (cl-incf (tui-process-buffer-state--internal new-proc-state))
         new-proc-state)))

(defun tui-use-process-buffer--process-filter (buffer-to-write set-proc-state)
  (lambda (_ delta)
    (with-current-buffer buffer-to-write
      (let ((inhibit-read-only t))
        (insert delta)))
    (funcall set-proc-state #'tui-use-process-buffer--signal-update)))

(defun tui-use-process-buffer--view-indirect-action (buffer buffer-name)
  (lambda ()
    (interactive)
    (pop-to-buffer
     (or (get-buffer buffer-name)
         (make-indirect-buffer buffer buffer-name t)))))

(defun tui-use-process-buffer (component command)
  (let* ((proc-state (tui-use-state component nil))
         (set-proc-state (cadr proc-state)))
    (tui-use-effect
     component
     (list command set-proc-state)
     (lambda ()
       (when command
         (let* ((stderr-buffer (tui-use-process-buffer--anon-buffer))
                (stdout-buffer (tui-use-process-buffer--anon-buffer))
                (_ (progn
                     (with-current-buffer stderr-buffer
                       (shell-mode))
                     (with-current-buffer stdout-buffer
                       (shell-mode))))
                (stderr-pipe-process
                 (make-pipe-process
                  :name (format "tui-process-stderr-%s" (string-join command "-"))
                  :buffer stderr-buffer
                  :noquery t
                  :filter (tui-use-process-buffer--process-filter
                           stderr-buffer
                           set-proc-state)
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
                           set-proc-state)
                  :sentinel (lambda (&rest ignored)
                              (funcall set-proc-state #'tui-use-process-buffer--signal-update)))))
           (funcall set-proc-state
                    (tui-process-buffer-state--create
                     :process process
                     :stdout-buffer stdout-buffer
                     :stderr-buffer stderr-buffer
                     :-internal 0))
           (lambda ()
             ;; could already be complete
             (ignore-errors
               (kill-process process))
             (ignore-errors
               (kill-process stderr-pipe-process))
             (ignore-errors (kill-buffer stderr-buffer))
             (ignore-errors (kill-buffer stdout-buffer)))))))
    (car proc-state)))


;; (tui-process-component--get-buffer-preview (current-buffer))
(defun tui-process-component--get-buffer-preview (buffer)
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (save-excursion
           (let* ((pt-max (point-max))
                  (tail-size 10)
                  (_ (progn
                       (goto-char pt-max)
                       (forward-line (- tail-size))))
                  (pt-beginning (point))
                  (num-lines-to-append
                   (- tail-size (count-lines pt-beginning pt-max)))
                  (suffix
                   (and (wholenump num-lines-to-append)
                        (make-list num-lines-to-append "\n"))))
             ;; this exposes a reconciliation bug.. see tui-process-test
             (string-join
              (cons (buffer-substring-no-properties pt-beginning pt-max)
                    suffix)))))))


(tui-defun-2 tui-process-component (&props process-buffer-state &this this)
  "Render a dashboard of the process, with buttons that take you to stdout/stderr buffers"
  (when process-buffer-state
    (let* ((process (tui-process-buffer-state-process process-buffer-state))
           (stderr-buffer (tui-process-buffer-state-stdout-buffer process-buffer-state))
           (stdout-buffer (tui-process-buffer-state-stderr-buffer process-buffer-state))
           (process-command (process-command process))
           (process-status (process-status process)))
      (tui-span
       (tui-heading (string-join process-command " "))
       "\n"
       (format "process-status: %s" process-status)
       (when (eql process-status 'run)
         (tui-span
          " "
          (tui-button
           :children "(click to kill)"
           :action (lambda ()
                     (interactive)
                     (ignore-errors
                       (kill-process process))))))
       (unless (zerop (buffer-size stdout-buffer))
         `("\n"
           "tail of stdout: "
           ,(tui-button
             :children "(click to widen)"
             :action (tui-use-process-buffer--view-indirect-action
                      stdout-buffer
                      (format "*%s*" (string-join `(,@process-command "stdout") "-"))))
           "\n"
           "------\n"
           ,(tui-process-component--get-buffer-preview stdout-buffer)
           "------\n"))
       (unless (zerop (buffer-size stderr-buffer))
         `("\n"
           "tail of stderr: "
           ,(tui-button
             :children "(click to widen)"
             :action (tui-use-process-buffer--view-indirect-action
                      stderr-buffer
                      (format "*%s*" (string-join `(,@process-command "stderr") "-"))))
           "\n"
           "------\n"
           ,(tui-process-component--get-buffer-preview stderr-buffer)
           "------\n"))
       ))))

(provide 'tui-use-process-buffer)
