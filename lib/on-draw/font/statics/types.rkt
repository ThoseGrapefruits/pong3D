#lang typed/racket/base

(require pict3d)

(provide (all-defined-out))

(struct Char-3D
  ([char : Char]
   [width : Flonum]
   ; This has to be a function so that we can parameterize color, material,
   ; emitted, etc when drawing.
   [draw : (-> Pict3D)]))

(module draw-memoized racket
  (require nested-hash pict3d)
  (provide draw-memoize)

  (define (draw-memoize char draw)
    (define cache-box (box (make-immutable-hasheq)))
    (λ ()
      (define cached-value
        (nested-hash-ref (unbox cache-box)
                         char (current-emitted) ; key
                         #:default #f))
      (cond [cached-value cached-value]
            [else (define drawn (draw))
                  (set-box! cache-box
                            (nested-hash-set (unbox cache-box)
                                             char (current-emitted) ; key
                                             drawn))
                  drawn]))))

(require/typed 'draw-memoized
  [draw-memoize (-> Char (-> Pict3D) (-> Pict3D))])

(: make-Char-3D-memoized (-> Char Flonum (-> Pict3D) Char-3D))
(define (make-Char-3D-memoized char width draw)
  (Char-3D char width (draw-memoize char draw)))
