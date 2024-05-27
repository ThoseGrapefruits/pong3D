#lang typed/racket/base

(require racket/list
         "./util.rkt"
         "../state/state.rkt")

(provide on-key-immediate-pause-menu)

(: on-key-immediate-pause-menu : State-Pause-Menu Natural Flonum String -> State)
(define (on-key-immediate-pause-menu s n t k)
  (cond
    [(just-pressed? s k "escape" "a")           (menu-go-out      s n t k   )]
    [(just-pressed? s k "space"  "d" "\n" "\r") (menu-go-in       s n t k   )]
    [(just-pressed? s k "up"     "w")           (menu-go-vertical s n t k -1)]
    [(just-pressed? s k "down"   "s")           (menu-go-vertical s n t k  1)]
    [else s]))

(: menu-go-out : State-Pause-Menu Natural Flonum String -> (U State-Pause-Menu State-Play))
(define (menu-go-out s n t k)
  (define menu (State-Pause-Menu-menu s))
  (define active-box (Menu-active-item menu))
  (define parent (Menu-Item-parent (unbox active-box)))
  (cond [(Menu-Item? parent)
         (set-box! active-box parent)
         (set-box! (Menu-active-since menu) t)
         s]
        [else (struct-copy
               State-Play (State-Pause-Menu-resume-state s)
               [t #:parent State t]
               [pause-state (struct-copy State-Pause-Menu s
                                         ; clear the old resume-state, will
                                         ; get reset if re-pause-menu
                                         [resume-state #f])])]))

(: menu-go-in : State-Pause-Menu Natural Flonum String -> (U State-Pause-Menu State-Play))
(define (menu-go-in s n t k)
  (define menu (State-Pause-Menu-menu s))
  (define active-box (Menu-active-item menu))
  (define first-child (first (Menu-Item-children (unbox active-box))))
  (cond [(Menu-Item? first-child)
         (set-box! active-box first-child)
         (set-box! (Menu-active-since menu) t)
         s]
        [else s]))

(: menu-go-vertical : State-Pause-Menu Natural Flonum String (U -1 1) -> State-Pause-Menu)
(define (menu-go-vertical s n t k offset)
  (define menu (State-Pause-Menu-menu s))
  (define active-box (Menu-active-item menu))
  (define active (unbox active-box))
  (define parent (unbox (Menu-Item-parent active)))
  (define siblings   (and (Menu-Item? parent) (Menu-Item-children parent)))
  (define index      (and siblings            (index-of siblings active)))
  (define index-new  (and index               (+ offset index)))
  (define active-new (and index-new           (list-ref siblings index-new)))
  (cond [(Menu-Item? active-new)
         (set-box! active-box active-new)
         (set-box! (Menu-active-since menu) t)
         s]
        [else s]))
