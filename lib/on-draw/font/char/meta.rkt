#lang typed/racket

(require "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt")

(provide (all-defined-out))

(define misc:unknown
  (Char-3D #\nul
           WIDTH-EM
           (λ () (placeholder-tall WIDTH-EM-3/8))))