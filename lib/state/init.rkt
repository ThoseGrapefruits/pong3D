#lang typed/racket/base

(require
  (only-in pict3d Pict3D Pos)
  (only-in pict3d
           dir
           empty-pict3d
           pos
           pos-y)
  (only-in racket/set set)
  "../config.rkt"
  "../on-draw/palette.rkt"
  "../util/ball.rkt"
  "../util/number.rkt"
  "../util/pid.rkt"
  "./menu.rkt"
  "./menu-item-types.rkt"
  "./menu-root-settings.rkt"
  "./state.rkt"
  "./syntax.rkt")

(provide state-reset-play
         state-start
         state-start-play)

(: state-reset-play : State Natural Flonum -> State-Play)
(define (state-reset-play s n t)
  (define s-play (state-start-play s t))
  (box-cas! (State-dt s-play) (unbox (State-dt s-play)) 0.0)
  (box-cas! (State-n s-play)  (unbox (State-n s-play))  n)
  (box-cas! (State-t s-play)  (unbox (State-t s-play))  t)
  s-play)

(: state-start : (->* ((U State #f)) (Flonum) State-Any))
(define (state-start s [t 0.0])
  (State-Main-Menu
   ; State
   (box 0.0)          ; dt
   (cons              ; mouse-pos-last
    (round (/ SCREEN-WIDTH-INIT 2))
    (round (/ SCREEN-HEIGHT-INIT 2)))
   (box 0)            ; n
   (box empty-pict3d) ; pict-last
   (set)              ; pressed
   (box t)            ; t
   #f                 ; trace-mouse
   #f                 ; trace-mouse/last
   #f                 ; trace-mouse-down
   (or                ; window-dims
    (and s (State-window-dims s))
    (cons
     (assert SCREEN-WIDTH-INIT index?)
     (assert SCREEN-HEIGHT-INIT index?)))

   ; State-Main-Menu
   (make-Menu ; menu
    (get-menu-root-main))))

(: state-start-play : (->* (State) (Flonum) State-Play))
(define (state-start-play s [t 0.0])
  (define ball (state-start-play-ball))
  (define ball-ppe (predict-ball-pos-ends-2 ball))
  (define predicted-ball-y (pos-y (car ball-ppe)))
  (State-transition
   State-Play s
   ball      ; ball
   ball-ppe  ; ball-predicted-pos-ends
   (Opponent ; opponent
    (make-pid #:tuning-p 0.2
              #:tuning-i 0.00007
              #:tuning-d 0.0002)
    predicted-ball-y) ; y
   #f        ; pause-state
   (Player   ; player
    3        ; lives
    0        ; score
    (box #f) ; score-last-frame
    1.0      ; score-multiplier
    0.0      ; y
    0.0      ; y-desired
    (make-pid #:tuning-p 0.3 ; pid
              #:tuning-i 0.00001
              #:tuning-d 0.00002))
   t))

(: state-start-play-ball : -> Ball)
(define (state-start-play-ball)
  (define y (* 1.5 (- 0.5 (random/0-1))))
  (Ball (dir -1.0 y 0.0)    ; dir
        (pos 0.0 0.0 0.0))) ; pos

(: get-menu-root-main : -> Menu-Item)
(define (get-menu-root-main)
  (make-Menu-Item
   #:label "pong3D"
   #:tag   'root-main
   #:children
   (list (make-Menu-Item
          #:color-active EMITTED-BLUE
          #:label "Start"
          #:tag   'start)
         (get-menu-root-settings 'root-main)
         (make-Menu-Item
          #:color-active EMITTED-YELLOW
          #:label "Exit"
          #:tag   'exit))))
