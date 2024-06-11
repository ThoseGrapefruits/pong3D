#lang typed/racket/base

(require pict3d
         "../../../util/nested-hash.rkt"
         "./types.rkt")

(provide make-Char-3D-memoized)

(define default-on-draw : On-Draw-Handler (λ (draw c i) (draw)))

(: draw-cache-box : (Boxof HashTableTop))
(define draw-cache-box (box (make-immutable-hasheq)))

(: draw-memoize : Char Draw-Raw -> Draw)
(define (draw-memoize char draw)
  (λ ([ondraw default-on-draw] [c char] [i 0])
    (ondraw
     (λ ()
       (define cache-key (list char (current-emitted)))
       (define draw-cache (unbox draw-cache-box))
       (define cached-value
         (nested-hash-ref* draw-cache
                           cache-key))
       (cond [cached-value cached-value]
             [else
              (define drawn (freeze (draw))) ; the freeze-draw cycle
              (set-box! draw-cache-box
                        (nested-hash-set* draw-cache
                                          cache-key
                                          drawn))
              drawn]))
     c
     i)))

(: make-Char-3D-memoized (-> Char Flonum Draw-Raw Char-3D))
(define (make-Char-3D-memoized char width draw)
  (Char-3D
   char                     ; char
   width                    ; width
   (draw-memoize char draw) ; draw
   draw))                   ; draw-raw
