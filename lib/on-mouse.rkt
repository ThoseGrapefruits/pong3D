#lang typed/racket/base

(require pict3d
         typed-compose
         racket/bool
         "./state/menu.rkt"
         "./state/state.rkt"
         "./state/syntax.rkt"
         "./state/updaters.rkt"
         "./util/tag.rkt"
         "./util/pid.rkt")

(provide on-mouse)

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  ((compose-n ; bottom-to-top
    (λ ([s : State]) (on-mouse-post s n t x y e))
    (λ ([s : State])
      (cond [(State-Play? s)      (on-mouse-play      s n t x y e)]
            [(State-Main-Menu? s) (on-mouse-main-menu s n t x y e)]
            [else s]))
    (λ ([s : State]) (on-mouse-pre s n t x y e)))
   s))

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "left-up") (on-mouse-main-menu-left-up s)]
        [else                   s]))

(: on-mouse-main-menu-left-up : State-Main-Menu -> State)
(define (on-mouse-main-menu-left-up s)
  (define mouse-down (State-trace-mouse-down s))
  (define trace      (State-trace-mouse      s))
  (define mouse-down-path (and mouse-down (surface-data-path mouse-down)))
  (define trace-path      (and trace      (surface-data-path trace)))

  (cond [(and mouse-down-path
              trace-path
              (path=? mouse-down-path trace-path))
         (struct-copy State-Main-Menu s
                      [menu (Menu-handle-activate s (State-Main-Menu-menu s))])]
        [else s]))

(: on-mouse-play : State-Play Natural Flonum Integer Integer String -> State)
(define (on-mouse-play s n t x y e)
  (cond [(string=? e "move")
         (define trace (State-trace-mouse/last s))
         (define player (State-Play-player s))
         (define player-pid (Player-y-pid player))
         (or (Player-y-desired player)
             (pid-reset! player-pid))
         (if (surface-data? trace)
             (struct-copy
              State-Play s
              [player (struct-copy
                       Player (State-Play-player s)
                       [y-desired (pos-y (surface-data-pos trace))])])
             s)]
        [else s]))

(: on-mouse-post : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-post s n t x y e)
  ((compose-n
    (λ ([s : State]) (on-mouse-post-left s n t x y e)))
   s))

(: on-mouse-post-left : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-post-left s n t x y e)
  (cond [(string=? e "left-up")
         (State-update-parent s
                              [trace-mouse-down #:parent State #f])]
        [else s]))

(: on-mouse-pre : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-pre s n t x y e)
  ((compose-n ; bottom-to-top
    (λ ([s : State]) (on-mouse-pre-left s n t x y e))
    (λ ([s : State]) (State-update-trace-mouse s x y))
  ) s))

(: on-mouse-pre-left : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-pre-left s n t x y e)
  (cond [(string=? e "left-down")
         (State-update-parent s
                              [trace-mouse-down #:parent State (State-trace-mouse s)])]
        [else s]))
