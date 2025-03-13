#lang typed/racket/base

(require
  pict3d
  racket/match
  "./state/state.rkt"
  "./state/syntax.rkt")

(provide on-resize)

(: on-resize : State-Any Natural Flonum Integer Integer -> State-Any)
(define (on-resize s n t width height)
  (assert width index?)
  (assert height index?)
  (State-update-parent s [window-dims #:parent State (cons width height)]))
