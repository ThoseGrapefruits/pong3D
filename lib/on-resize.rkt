#lang typed/racket/base

(require
  (only-in "./state/syntax.rkt" State-update-parent)
  (only-in "./state/state.rkt" State State-Any))

(provide on-resize)

(: on-resize : State-Any Natural Flonum Integer Integer -> State-Any)
(define (on-resize s n t width height)
  (assert width index?)
  (assert height index?)
  (State-update-parent s [window-dims #:parent State (cons width height)]))
