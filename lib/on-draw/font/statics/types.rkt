#lang typed/racket/base

(require (only-in pict3d Pict3D))

(provide (struct-out Char-3D)
  Draw
  Draw-Raw
  On-Char-Handler
  On-Draw-Handler)

(define-type Draw-Raw (-> Pict3D))
(define-type On-Char-Handler (-> Pict3D Char Integer Pict3D))
(define-type On-Draw-Handler (-> Draw-Raw Char Integer Pict3D))

(define-type Draw (->* () (On-Draw-Handler Char Integer) Pict3D))

(struct Char-3D
  ([char : Char]
   [width : Flonum]
   ; This has to be a function so that we can parameterize color, material,
   ; emitted, etc when drawing.
   [draw : Draw]
   [draw-raw : Draw-Raw])
  #:transparent)
