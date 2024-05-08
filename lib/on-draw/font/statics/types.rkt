#lang typed/racket/base

(require pict3d)

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

(module draw-memoized racket/base
  (require nested-hash pict3d)
  (provide draw-memoize)

  (define (draw-memoize char draw)
    (define cache-box (box (make-immutable-hasheq)))
    (λ ([ondraw apply] [c char] [i 0])
      (ondraw (λ ()
                (define cached-value
                  (nested-hash-ref (unbox cache-box)
                                   char (current-emitted) ; key
                                   #:default #f))
                (cond [cached-value cached-value]
                      [else (define drawn (freeze (draw))) ; the freeze-draw cycle
                            (set-box! cache-box
                                      (nested-hash-set (unbox cache-box)
                                                       char (current-emitted) ; key
                                                       drawn))
                            drawn])) c i))))

(require/typed 'draw-memoized
  [draw-memoize (-> Char Draw-Raw Draw)])

(: make-Char-3D-memoized (-> Char Flonum Draw-Raw Char-3D))
(define (make-Char-3D-memoized char width draw)
  (Char-3D
   char                     ; char
   width                    ; width
   (draw-memoize char draw) ; draw
   draw))                   ; draw-raw
