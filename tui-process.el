;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)

(cl-defstruct (tui-process-state (:constructor tui-process-state--create)
                                 (:copier nil))
  process                          ; the process that is ongoing
  stdout-deltas                    ; filter function list aggregation, lifo
  stderr-deltas                    ; ibid
  dependencies)                    ; used by tui-process-create to determine when to re-create the process

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

(defun tui-process--plist-overwrite (key plist overwriter)
  (let* ((prev (plist-get plist key))
         (overwritten (funcall overwriter prev))
         (copy (copy-sequence plist)))
    (plist-put plist key overwritten)))

;; it's likely possible to infer non-special dependencies if we go through the trouble of writing a macro
;; but react developers are used to defining dependencies, so no biggie
(cl-defun tui-process-create (component state-key command-creator dependencies)
  (let ((prev-state (plist-get (tui-get-state component) state-key)))
    (if (or (not prev-state)
            (not (eq dependencies (tui-process-state-dependencies prev-state))))
        (let* ((command (funcall command-creator))
               (stdout-filter (lambda (proc text)
                                (tui-set-state
                                 component
                                 (tui-process--plist-overwrite
                                  state-key
                                  ;; it is not safe to reference prev-state here                               
                                  (tui-get-state component)
                                  (lambda (proc-state)
                                    ;; update the stdout deltas, keep everything else
                                    (tui-process-state--create
                                     :process (tui-process-state-process proc-state)
                                     :stdout-deltas (cons text (tui-process-state-stdout-deltas proc-state))
                                     :stderr-deltas (tui-process-state-stderr-deltas proc-state)
                                     :dependencies (tui-process-state-dependencies proc-state)))))))
               (stderr-filter (lambda (proc text)
                                (tui-set-state
                                 component
                                 (tui-process--plist-overwrite
                                  state-key
                                  ;; it is not safe to reference prev-state here
                                  (tui-get-state component)
                                  (lambda (proc-state)
                                    ;; update the stderr deltas, keep everything else
                                    (tui-process-state--create
                                     :process (tui-process-state-process proc-state)
                                     :stdout-deltas (tui-process-state-stdout-deltas proc-state)
                                     :stderr-deltas (cons text (tui-process-state-stderr-deltas proc-state))
                                     :dependencies (tui-process-state-dependencies proc-state)))))))
               (new-proc-state
                (tui-process-state--create
                 :process (tui-process--execute command stdout-filter stderr-filter)
                 :stdout-deltas '()
                 :stderr-deltas '()
                 :dependencies dependencies)))
          (tui-set-state
           component
           (tui-process--plist-overwrite
            state-key
            (tui-get-state component)
            (lambda (prev-state)
              ;; tear down the old one as we're replacing it with a new one
              (when prev-state
                (kill-process (tui-process-state-process prev-state)))
              new-proc-state)))
          new-proc-state)
      prev-state)))

(tui-defun-2 my-component (&this this)
  "documentation is good"
  (let ((proc (tui-process-create this :example (lambda () '("logger" "-s" "'please clap'")) nil)))
    (tui-div
     (format "stdout: %s\n" (string-join (reverse (tui-process-state-stdout-deltas proc))))
     (format "stderr: %s" (string-join (reverse (tui-process-state-stderr-deltas proc)))))))


(let* ((buffer (get-buffer-create "*my-component*"))
       (component (my-component)))
  (tui-render-element
   (tui-buffer
    :buffer buffer
    component))
  (switch-to-buffer buffer))


(provide 'tui-process)
