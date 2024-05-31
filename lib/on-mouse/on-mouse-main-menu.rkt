#lang typed/racket/base

(require pict3d
         racket/list
         "../menus/main-menu.rkt"
         "../state/menu.rkt"
         "../state/state.rkt"
         "../util/tag.rkt")

(provide on-mouse-main-menu)

(: on-mouse-main-menu : State-Main-Menu Natural Flonum Integer Integer String -> State)
(define (on-mouse-main-menu s n t x y e)
  (cond [(string=? e "move")    (on-mouse-main-menu-move s n t)]
        [(string=? e "left-up") (on-mouse-main-menu-left-up s n t)]
        [else                   s]))

(: on-mouse-main-menu-left-up : State-Main-Menu Natural Flonum -> State)
(define (on-mouse-main-menu-left-up s n t)
  (define mouse-down (State-trace-mouse-down s))
  (define trace      (State-trace-mouse      s))
  (define mouse-down-path (and mouse-down (surface-data-path mouse-down)))
  (define trace-path      (and trace      (surface-data-path trace)))

  (cond [(and mouse-down-path
              trace-path
              (not (empty? trace-path))
              (path=? mouse-down-path trace-path))
         (set-box! (Menu-active-path (State-Main-Menu-menu s)) trace-path)
         (Main-Menu-activate s n t trace-path)]
        [else s]))

(: on-mouse-main-menu-move : State-Main-Menu Natural Flonum -> State)
(define (on-mouse-main-menu-move s n t)
  (define menu            (State-Main-Menu-menu s))
  (define trace           (State-trace-mouse      s))
  (define trace-path      (and trace (surface-data-path trace)))
  (define hover-path      (unbox (Menu-hovered-path menu)))
  (define hover-item      (and hover-path (Menu-ref menu hover-path)))
  (define trace-item      (and trace-path (Menu-ref menu trace-path)))

  (cond [(and trace-path
              trace-item
              (or (not hover-path)
                  (not (path=? hover-path trace-path))))
         (set-box! (Menu-hovered-path menu) trace-path)
         (Menu-Item-active-transition! hover-item trace-item t)
         s]
        [else s]))
