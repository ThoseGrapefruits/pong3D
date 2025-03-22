#lang typed/racket/base

(require
  racket/list
  (only-in racket/match match-define)
  "./game-score.rkt"
  "../util/number.rkt"
  pict3d)

(provide render-player-score)

(: render-player-score : Nonnegative-Integer -> Pict3D)
(define (render-player-score score)
  (combine
   (for/list : (Listof Pict3D)
     ([ score-section SCORE-SECTIONS ])
     (match-define (Score-Section color-emitted y place-low place-high) score-section)
     (parameterize ([current-emitted color-emitted])
       (combine
        (for/list : (Listof Pict3D)
          ([ n (range 0.0 10) ])
          (pipe (pos (* n 0.03) y 0.0) (dir 0.01 0.01 0.001)
                #:top-radii (interval 6/10 1)
                #:bottom-radii (interval 6/10 1)))
        (for/list : (Listof Pict3D)
          ([n (range 0.0 (get-number-place score place-low place-high))])
          (sphere (pos (* n 0.03) y 0.0) 0.01)))))))
