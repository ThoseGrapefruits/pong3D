#lang typed/racket/base

(require "../menus/main-menu.rkt"
         "../state/state.rkt"
         "./menu.rkt")

(provide on-mouse-main-menu)

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State-Any)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "move")    (on-mouse-main-menu-move s n t)]
        [(string=? e "left-up") (on-mouse-main-menu-left-up s n t)]
        [else                   s]))

(: on-mouse-main-menu-left-up : State-Main-Menu Natural Flonum -> State-Any)
(define (on-mouse-main-menu-left-up s n t)
  (define menu (State-Main-Menu-menu s))
  (Menu-on-mouse-left-up s menu n t Main-Menu-activate))

(: on-mouse-main-menu-move : State-Main-Menu Natural Flonum -> State-Any)
(define (on-mouse-main-menu-move s n t)
  (define menu (State-Main-Menu-menu s))
  (Menu-on-mouse-move s menu n t))
