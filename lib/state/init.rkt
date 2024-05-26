#lang typed/racket/base

(require
  pict3d
  racket/set
  "../util/ball.rkt"
  "../util/pid.rkt"
  "../util/number.rkt"
  "../config.rkt"
  "./state.rkt")

(provide (all-defined-out))

(: state-reset : State Natural Flonum -> State-Play)
(define (state-reset s n t)
  (struct-copy
   State-Play (state-start t)
   [dt #:parent State (State-dt s)]
   [n  #:parent State (State-n  s)]
   [t  #:parent State (State-t  s)]))

(: state-start : (->* () (Flonum) State))
(define (state-start [t 0.0])
  (define ball (state-start-game-play-ball))
  (define predicted-ball-y (pos-y (predict-ball-pos ball)))
  (define ball-ppe (predict-ball-pos-ends-2 ball))
  (State-Play
   ; State
   0.0                ; dt
   (cons              ; mouse-pos-last
    (round (/ SCREEN-WIDTH 2))
    (round (/ SCREEN-HEIGHT 2)))
   0                  ; n
   (box empty-pict3d) ; pict-last
   (set)              ; pressed
   t                  ; t
   #f                 ; trace-mouse
   #f                 ; trace-mouse/last
   #f                 ; trace-mouse-down
   (cons              ; window-dims
    (if (index? SCREEN-WIDTH)  SCREEN-WIDTH  0)
    (if (index? SCREEN-HEIGHT) SCREEN-HEIGHT 0))

   ; State-Play
   ball      ; ball
   ball-ppe  ; ball-predicted-pos-ends
   (Opponent ; opponent
    predicted-ball-y) ; y
   #f        ; pause-state
   (Player   ; player
    3                        ; lives
    0                        ; score
    1.0                      ; score-multiplier
    0.0                      ; y
    0.0                      ; y-desired
    (make-pid #:tuning-p 0.3 ; pid
              #:tuning-i 0.00001
              #:tuning-d 0.00002))
   t))   ; start-t

(define (state-start-game-play-ball)
  (define y (* 1.5 (- 0.5 (random-0-1))))
  (Ball (dir -1.0 y 0.0)    ; dir
        (pos 0.0 0.0 0.0))) ; pos
