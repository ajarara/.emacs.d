;; -*- lexical-binding: t -*-

(require 'tui-hooks)

;; experimental

;; this has some significant problems:
;; - let binding symbols are incorrectly picked up
;; - (car ls) depends on ls, it should depend only on (car ls)
;;   - admittedly this problem cannot be solved by inspecting closures
(defmacro tui-hooks-x--infer-dependencies (body)
  `(list ,@(cl-loop for sym in (flatten-tree body)
                    collect `(ignore-errors ,sym))))

(defmacro tui-use-inferred-effect (component &rest effect)
  (declare (indent defun))
  (let ((dependencies `(tui-hooks-x--infer-dependencies ,effect)))
     `(tui-use-effect ,component ,dependencies (lambda () ,@effect))))

(defmacro tui-use-inferred-memo (component &rest compute)
  (declare (indent defun))
  (let ((dependencies `(tui-hooks-x--infer-dependencies ,compute)))
    `(tui-use-memo ,component ,dependencies (lambda () ,@compute))))

(defmacro tui-use-inferred-callback (component callback)
  (let ((dependencies `(tui-hooks-x--infer-dependencies ,callback)))
    `(tui-use-callback ,component ,dependencies ,callback)))

(provide 'tui-hooks-x)
