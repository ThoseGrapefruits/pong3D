#lang typed/racket/base

(require
  pict3d
  racket/list
  racket/set
  "../config.rkt"
  "../util/ball.rkt"
  "../util/number.rkt"
  "../util/pid.rkt"
  "./menu.rkt"
  "./state.rkt"
  "./syntax.rkt")

(provide state-reset-play
         state-start
         state-start-play)

(: state-reset-play : State Natural Flonum -> State-Play)
(define (state-reset-play s n t)
  (struct-copy
   State-Play (state-start-play s t)
   [dt #:parent State 0.0]
   [n  #:parent State n]
   [t  #:parent State t]))

(: state-start : (->* () (Flonum) State))
(define (state-start [t 0.0])
  (State-Main-Menu
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

   ; State-Main-Menu
   (make-Menu (make-Menu-Item
               #:children (list)
               #:label "Pong"
               #:tag 'root-main))))

(: state-start-play : (->* (State) (Flonum) State-Play))
(define (state-start-play s [t 0.0])
  (define ball (state-start-play-ball))
  (define ball-ppe (predict-ball-pos-ends-2 ball))
  (define predicted-ball-y (pos-y (second ball-ppe)))
  (State-transition
   State-Play s
   ball      ; ball
   ball-ppe  ; ball-predicted-pos-ends
   (Opponent ; opponent
    predicted-ball-y) ; y
   #f        ; pause-state
   (Player   ; player
    3   ; lives
    0   ; score
    1.0 ; score-multiplier
    0.0 ; y
    0.0 ; y-desired
    (make-pid #:tuning-p 0.3 ; pid
              #:tuning-i 0.00001
              #:tuning-d 0.00002))
   t))

(: state-start-play-ball : -> Ball)
(define (state-start-play-ball)
  (define y (* 1.5 (- 0.5 (random/0-1))))
  (Ball (dir -1.0 y 0.0)    ; dir
        (pos 0.0 0.0 0.0))) ; pos
