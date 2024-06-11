#lang typed/racket/base

(require pict3d
         "../../../util/nested-hash.rkt")

(provide (all-defined-out))

(define-type Draw-Raw (-> Pict3D))
(define-type On-Draw-Handler (-> Draw-Raw Char Integer Pict3D))
(define-type On-Char-Handler (-> Pict3D Char Integer Pict3D))

(define-type Draw (->* () (On-Draw-Handler Char Integer) Pict3D))

(struct Char-3D
  ([char : Char]
   [width : Flonum]
   ; This has to be a function so that we can parameterize color, material,
   ; emitted, etc when drawing.
   [draw : Draw]
   [draw-raw : Draw-Raw]))

(: draw-cache-box : (Boxof HashTableTop))
(define draw-cache-box (box (make-immutable-hasheq)))

(: draw-memoize : Char Draw-Raw -> Draw)
(define (draw-memoize char draw)
  (define default-on-draw : On-Draw-Handler (λ (draw c i) (draw)))
  (λ ([ondraw default-on-draw] [c char] [i 0])
    (ondraw (λ ()
              (define cache-key (list char (current-emitted)))
              (define cached-value
                (nested-hash-ref* (unbox draw-cache-box)
                                  cache-key
                                  #:default #f))
              (cond [cached-value cached-value]
                    [else (define drawn (freeze (draw))) ; the freeze-draw cycle
                          (set-box! draw-cache-box
                                    (nested-hash-set* (unbox draw-cache-box)
                                                      cache-key
                                                      drawn))
                          drawn])) c i)))

(: make-Char-3D-memoized (-> Char Flonum Draw-Raw Char-3D))
(define (make-Char-3D-memoized char width draw)
  (Char-3D
   char                     ; char
   width                    ; width
   (draw-memoize char draw) ; draw
   draw))                   ; draw-raw
