#lang typed/racket

(require pict3d
         "../statics/measurements.rkt"
         "../statics/types.rkt")

(provide (all-defined-out))

(define ws:space
  (Char-3D #\space WIDTH-EM-1/2 (λ () empty-pict3d)))

(define ws:tab
  (Char-3D #\tab WIDTH-EM-2 (λ () empty-pict3d)))
