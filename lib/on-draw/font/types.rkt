#lang typed/racket

(require pict3d)

(provide (all-defined-out))

(define-struct Char-3D
  ([width : Flonum]
   ; This has to be a function so that we can parameterize color, material,
   ; emitted, etc when drawing.
   [draw : (-> Pict3D)]))