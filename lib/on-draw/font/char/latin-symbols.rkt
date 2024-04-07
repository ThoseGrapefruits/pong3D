#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt")

(provide (all-defined-out))

(define symbol:?
  (Char-3D #\?
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))

(define symbol:dot
  (Char-3D #\.
           WIDTH-EM-1/4
           (λ () (sphere LINE/BASE/CENTER-1/4 WIDTH-EM-1/16))))