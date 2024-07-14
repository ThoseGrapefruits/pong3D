#lang typed/racket/base

(require pict3d
         typed-compose
         "../state/state.rkt"
         "../state/syntax.rkt"
         "../state/updaters.rkt"
         "../util/pid.rkt"
         "./on-mouse-main-menu.rkt"
         "./on-mouse-pause-menu.rkt")

(provide on-mouse)

(: on-mouse : State-Any Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse s n t x y e)
  ((compose-n ; bottom-to-top
    (λ ([s : State-Any]) : State-Any
      (on-mouse-post s n t x y e))
    (λ ([s : State-Any]) : State-Any
      (cond [(State-Play? s)       (on-mouse-play       s n t x y e)]
            [(State-Main-Menu? s)  (on-mouse-main-menu  s n t x y e)]
            [(State-Pause-Menu? s) (on-mouse-pause-menu s n t x y e)]
            [else s]))
    (λ ([s : State-Any]) : State-Any
      (on-mouse-pre s n t x y e)))
   s))

(: on-mouse-play : State-Play Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-play s n t x y e)
  (cond [(string=? e "move")
         (define trace (State-trace-mouse/last s))
         (define player (State-Play-player s))
         (define player-pid (Player-y-pid player))
         (unless (Player-y-desired player)
           (pid-reset! player-pid))
         (if (surface-data? trace)
             (struct-copy
              State-Play s
              [player (struct-copy
                       Player (State-Play-player s)
                       [y-desired (pos-y (surface-data-pos trace))])])
             s)]
        [else s]))

(: on-mouse-post : State-Any Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-post s n t x y e)
  ((compose-n
    (λ ([s : State-Any]) : State-Any
      (on-mouse-post-left s n t x y e)))
   s))

(: on-mouse-post-left : State-Any Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-post-left s n t x y e)
  (cond [(string=? e "left-up")
         (State-update-parent s
                              [trace-mouse-down #:parent State #f])]
        [else s]))

(: on-mouse-pre : State-Any Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-pre s n t x y e)
  ((compose-n ; bottom-to-top
    (λ ([s : State-Any]) : State-Any
      (on-mouse-pre-left s n t x y e))
    (λ ([s : State-Any]) : State-Any
      (State-update-trace-mouse s x y))
  ) s))

(: on-mouse-pre-left : State-Any Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-pre-left s n t x y e)
  (cond [(string=? e "left-down")
         (State-update-parent s
                              [trace-mouse-down #:parent State (State-trace-mouse s)])]
        [else s]))
