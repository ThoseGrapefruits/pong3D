#lang typed/racket/base

(require pict3d
         "./state/state.rkt"
         "./state/updaters.rkt"
         "./util/pid.rkt")

(provide on-mouse)

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  (define s-pre (on-mouse-pre s n t x y e))
  (on-mouse-post
   (cond [(State-Play? s-pre) (on-mouse-play s-pre n t x y e)]
         [(State-Main-Menu? s-pre) (on-mouse-main-menu s-pre n t x y e)]
         [else s-pre])
   n t x y e))

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "move") s]
        [else s]))

(: on-mouse-play : State-Play Natural Flonum Integer Integer String -> State)
(define (on-mouse-play s n t x y e)
  (cond [(string=? e "move")
         (define trace (State-mouse-trace-last s))
         (define player (State-Play-player s))
         (define player-pid (Player-y-pid player))
         (or (Player-y-desired player)
             (pid-reset! player-pid))
         (if (surface-data? trace)
             (struct-copy
              State-Play s
              [player (struct-copy
                       Player (State-Play-player s)
                       ; TODO change math to match changing screen sizes
                       [y-desired (pos-y (surface-data-pos trace))])])
             s)]
        [else s]))

(: on-mouse-post : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-post s n t x y e) s)

(: on-mouse-pre : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-pre s n t x y e)
  (State-update-mouse-trace s x y))
