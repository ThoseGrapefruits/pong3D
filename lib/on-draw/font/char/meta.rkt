#lang typed/racket

(require "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt")

(provide (all-defined-out))

(define misc:unknown
  (make-Char-3D-memoized
#\nul
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))