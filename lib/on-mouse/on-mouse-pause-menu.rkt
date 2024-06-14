#lang typed/racket/base

(require "../menus/navigation.rkt"
         "../menus/pause-menu.rkt"
         "../state/state.rkt"
         "./menu.rkt")

(provide on-mouse-pause-menu)

(: on-mouse-pause-menu : State-Pause-Menu Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-pause-menu s n t x y e)
  (cond [(string=? e "move")    (on-mouse-pause-menu-move s n t)]
        [(string=? e "left-up") (on-mouse-pause-menu-left-up s n t)]
        [else                   s]))

(: on-mouse-pause-menu-left-up : State-Pause-Menu Natural Flonum -> State-Any)
(define (on-mouse-pause-menu-left-up s n t)
  (define menu (State-Pause-Menu-menu s))
  (define on-activate : (Menu-On-Activate State-Pause-Menu)
    (λ (s n t _)
      (Pause-Menu-go-in s n t 'hover)))
  (Menu-on-mouse-left-up s menu n t on-activate))

(: on-mouse-pause-menu-move : State-Pause-Menu Natural Flonum -> State-Any)
(define (on-mouse-pause-menu-move s n t)
  (define menu (State-Pause-Menu-menu s))
  (Menu-on-mouse-move s menu n t))
