;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)

;; infer dependencies in closures
;; four candidates:
;; - figure out how to inspect the closures passed (despite the info pages suggesting this isn't a good idea)
;; - inspect the fully macroexpanded body and infer dependencies through checking all the different builtin let forms
;; - byte compile the body and look at the constants section (we risk re-running effects if this is overzealous)
;; - use cconv-analyze-form (this looks for free variables which is exactly what we want)
         
     

