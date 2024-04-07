#lang typed/racket/base

(require pict3d
         "../statics/measurements.rkt")

(provide quad-thicc)

; QUAD-THICC
; A quad with sides and a top that extend back on the z-axis by DEPTH-Z
(: quad-thicc : Pos Pos Pos Pos -> Pict3D)
(define (quad-thicc c1 c2 c3 c4)
  ; We treat the given positions as center positions to make alignment w/ other
  ; shapes easier
  (define c1-front (pos+ c1 -z DEPTH-Z-1/2))
  (define c2-front (pos+ c2 -z DEPTH-Z-1/2))
  (define c3-front (pos+ c3 -z DEPTH-Z-1/2))
  (define c4-front (pos+ c4 -z DEPTH-Z-1/2))
  (define c1-back (pos+ c1 +z DEPTH-Z-1/2))
  (define c2-back (pos+ c2 +z DEPTH-Z-1/2))
  (define c3-back (pos+ c3 +z DEPTH-Z-1/2))
  (define c4-back (pos+ c4 +z DEPTH-Z-1/2))

  (combine
   ; front
   (quad c1-front c2-front c3-front c4-front)
   ; sides
   ; note: order matters here, only one side is visible
   (quad c1-back c2-back c2-front c1-front)
   (quad c2-back c3-back c3-front c2-front)
   (quad c3-back c4-back c4-front c3-front)
   (quad c4-back c1-back c1-front c4-front)))