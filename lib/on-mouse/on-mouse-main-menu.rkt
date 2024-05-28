#lang typed/racket/base

(require pict3d
         "../menus/main-menu.rkt"
         "../state/state.rkt"
         "../util/tag.rkt")

(provide on-mouse-main-menu)

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "left-up") (on-mouse-main-menu-left-up s n t)]
        [else                   s]))

(: on-mouse-main-menu-left-up : State-Main-Menu Natural Flonum -> State)
(define (on-mouse-main-menu-left-up s n t)
  (define mouse-down (State-trace-mouse-down s))
  (define trace      (State-trace-mouse      s))
  (define mouse-down-path (and mouse-down (surface-data-path mouse-down)))
  (define trace-path      (and trace      (surface-data-path trace)))

  (cond [(and mouse-down-path
              trace-path
              (path=? mouse-down-path trace-path))
         (set-box! (Menu-active-path (State-Main-Menu-menu s)) trace-path)
         (Main-Menu-go-in s n t)]
        [else s]))
