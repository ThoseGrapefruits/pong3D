#lang typed/racket/base

(require pict3d
         "../statics/measurements.rkt"
         "../statics/memoize.rkt")

(provide (all-defined-out))

(define ws:space
  (make-Char-3D-memoized
#\space WIDTH-EM-1/2 (λ () empty-pict3d)))

(define ws:tab
  (make-Char-3D-memoized
#\tab WIDTH-EM-2 (λ () empty-pict3d)))
