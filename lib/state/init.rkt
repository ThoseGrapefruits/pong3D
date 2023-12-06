#lang typed/racket

(require
  pict3d
  "../util/ball/ball-prediction.rkt"
  "../util/number/index.rkt"
  "./state.rkt")

(provide (all-defined-out))

(: state-reset : State Natural Flonum -> State-Play)
(define (state-reset s n t)
  (struct-copy
   State-Play (state-start t)
   [dt #:parent State (State-dt s)]
   [n  #:parent State (State-n s)]
   [t  #:parent State (State-t s)]))

(: state-start : (->* () (Flonum) State))
(define (state-start [t 0.0])
  (define ball (state-start-game-play-ball))
  (define predicted-ball-y (pos-y (predict-ball-pos ball)))
  (define ball-ppe (predict-ball-pos-ends-2 ball))
  (State-Play
   ; State
   0.0   ; dt
   0     ; n
   (set) ; pressed
   t     ; t

   ; State-Play
   ball      ; ball
   ball-ppe  ; ball-predicted-pos-ends
   (Opponent ; opponent
    predicted-ball-y) ; y
   (Player   ; player
    3    ; lives
    0    ; score
    1.0  ; score-multiplier
    0.0) ; y
   t))   ; start-t

(define (state-start-game-play-ball)
  (Ball (dir -1.0 (* 1.5 (- 0.5 (random-0-1))) 0.0)
        (pos 0.0 0.0 0.0)))