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

(: state-start : (->* ((U State #f)) (Flonum) State-Any))
(define (state-start s [t 0.0])
  (State-Main-Menu
   ; State
   0.0                ; dt
   (cons              ; mouse-pos-last
    (round (/ SCREEN-WIDTH-INIT 2))
    (round (/ SCREEN-HEIGHT-INIT 2)))
   0                  ; n
   (box empty-pict3d) ; pict-last
   (set)              ; pressed
   t                  ; t
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
    (get-main-menu-root))))

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
    1   ; lives
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

(: get-main-menu-root : -> Menu-Item)
(define (get-main-menu-root)
  (make-Menu-Item
   #:label "pong3D"
   #:tag   'root-main
   #:children
   (list (make-Menu-Item
          #:color-active EMITTED-BLUE
          #:label "Start"
          #:tag   'start)
         (make-Menu-Item
          #:color-active EMITTED-PURPLE
          #:label "Settings"
          #:tag   'settings
          #:children
          (list (make-Menu-Item
                 #:color-active EMITTED-BLUE
                 #:label "Display"
                 #:tag   'display)
                (make-Menu-Item
                 #:color-active EMITTED-PURPLE
                 #:label "Sound"
                 #:tag   'sound
                 #:children
                 (list (make-Menu-Item
                   #:color-active EMITTED-BLUE
                   #:label "Main volume"
                   #:tag   'volume-main
                   #:type  (make-Menu-Item-Type-Slider-Flonum
                            #:key 'volume-main
                            #:decimal-digits 1
                            #:min 0.0
                            #:max 1.0))))))
         (make-Menu-Item
          #:color-active EMITTED-YELLOW
          #:label "Exit"
          #:tag   'exit))))
