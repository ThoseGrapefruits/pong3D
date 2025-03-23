#lang typed/racket/base

(require (only-in pict3d emitted))

(provide EMITTED-BLUE
         EMITTED-PURPLE
         EMITTED-WHITE
         EMITTED-YELLOW)

(define EMITTED-BLUE (emitted 0.4 0.6 1.0 2.0))

(define EMITTED-PURPLE (emitted 0.75 0.5 1.0 2.0))

(define EMITTED-WHITE (emitted 1.0 1.0 0.95 2.0))

(define EMITTED-YELLOW (emitted 1.0 0.8 0.0 2.0))
