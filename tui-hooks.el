;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)
(require 'generator)

;; assuming users want the hook state to be in the state of the component

(defun tui-use-effect (effect dependencies))

(defun tui-use-state (state))

(defun tui-use-ref ())

(defun tui-use-memo ())

(defun tui-use-callback ())

(cl-defstruct (tui-hooks--state-reference (:constructor tui-hooks--state-reference-create)
                                          (:copier nil))
  current)

(cl-defstruct (tui-hooks--dependencies-reference (:constructor tui-hooks--dependencies-reference-create)
                                                 (:copier nil)
                                                 (:include tui-hooks--state-reference))
  dependencies)

(cl-defstruct (tui-hooks--effect-reference (:constructor tui-hooks--effect-reference-create)
                                           (:copier nil)
                                           (:include tui-hooks--dependencies-reference)))

(cl-defstruct (tui-hooks--state (:constructor tui-hooks--state-create)
                                (:copier nil))
  component
  reference-index
  references)


(cl-defgeneric tui-hooks-advance (component)
  "Statefully return the current tui-hooks--state for the component. Hooks _must_ call this _exactly_ once")

(cl-defgeneric tui-hooks-get (state)
  "Return the current reference")

(cl-defgeneric tui-hooks-set (state next-reference)
  "Set the next reference, causing an update for the component. Can be called anytime during the lifetime of the component")

(cl-defmethod (tui-hooks-advance ((component tui-component)))
  (let* ((hook-state (plist-get (tui--get-state component) :tui-hooks--state))
         (next-hook-state (tui-hooks--state-create
                           :component (tui-hooks--state-component hook-state)
                           :reference-index (1+ (tui-hooks--state-reference-index hook-state))
                           :references (tui-hooks--state-reference references))))
    (tui--set-state component
                    (list :tui-hooks--state next-hook-state) t)
    hook-state))

(cl-defmethod tui-hooks-get ((state tui-hooks--state))
  (let* ((curr-idx (tui-hooks--state-reference-index state)))
    (nth idx (tui-hooks--state-references state))))

(cl-defmethod tui-hooks-set ((state tui-hooks--state) reference)
  (let ((idx (tui-hooks--state-reference-index state))
        (component (tui-hooks--state-reference-component component)))
    (tui--set-state
     component
     (lambda (prev-component-state)
       (let* ((prev-hook-state (plist-get prev-component-state :tui-hooks--state))
              (prev-references (tui-hooks--state-references prev-hook-state))
              (updated-references
               (tui-hooks--replace-and-pad-if-needed idx reference prev-references))
              (updated-state
               (tui-hooks--state-create
                :component component
                :reference-index (tui-hooks--state-reference-index prev-hook-state)
                :references updated-references)))
         (list :tui-hooks--state updated-state))))))

;; (tui-hooks--replace-and-pad-if-needed 0 "my-ref" '())
;; (tui-hooks--replace-and-pad-if-needed 1 "my-ref" '())
;; (tui-hooks--replace-and-pad-if-needed 0 "my-ref" '("old-ref"))
;; (tui-hooks--replace-and-pad-if-needed 1 "my-ref" '("front-ref" "to-be-replaced" "old-ref"))
(defun tui-hooks--replace-and-pad-if-needed (idx ref ls)
  (declare 'pure)
  "Replace ref in list for position idx, padding if needed"
  (let ((idx-relative-to-head (- idx (length ls))))
    (if (wholenump idx-relative-to-head)
        (append (list ref)
                (make-list idx-relative-to-head nil)
                ls)
      (append
       (cl-subseq ls 0 (- -1 idx-relative-to-head))
       (list ref)
       (cl-subseq ls (- idx-relative-to-head))))))