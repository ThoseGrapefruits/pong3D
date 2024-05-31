#lang typed/racket/base

(require racket/list
         "../state/menu.rkt"
         "../state/state.rkt"
         "../util/list.rkt")

(provide Pause-Menu-go-out
         Pause-Menu-go-in
         Pause-Menu-go-vertical)

(: Pause-Menu-go-out : State-Pause-Menu Natural Flonum -> State-Any)
(define (Pause-Menu-go-out s n t)
  (define menu (State-Pause-Menu-menu s))
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define parent (and active-menu-item
                      (Menu-Item-parent active-menu-item)))
  (cond [(Menu-Item? parent)
         (set-box! active-path-box (drop-right active-path 1))
         (Menu-Item-active-transition! active-menu-item parent t)
         s]
        [else (struct-copy
               State-Play (State-Pause-Menu-resume-state s)
               [t #:parent State t]
               [pause-state (struct-copy State-Pause-Menu s
                                         ; clear the old resume-state, will
                                         ; get reset if re-pause-menu
                                         [resume-state #f])])]))

(: Pause-Menu-go-in : State-Pause-Menu Natural Flonum -> (U State-Pause-Menu State-Play))
(define (Pause-Menu-go-in s n t)
  (define menu (State-Pause-Menu-menu s))
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define active-children (and active-menu-item
                               (Menu-Item-children active-menu-item)))
  (define first-child (and active-children
                           (not (empty? active-children))
                           (first active-children)))
  (cond [(Menu-Item? first-child)
         (set-box! active-path-box (append active-path
                                           (list (Menu-Item-tag first-child))))
         (Menu-Item-active-transition! active-menu-item first-child t)
         s]
        [else s]))

(: Pause-Menu-go-vertical : State-Pause-Menu Natural Flonum (U -1 1) -> State-Pause-Menu)
(define (Pause-Menu-go-vertical s n t offset)
  (define menu (State-Pause-Menu-menu s))
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define parent     (and active-menu-item    (unbox (Menu-Item-parent active-menu-item))))
  (define siblings   (and (Menu-Item? parent) (Menu-Item-children parent)))
  (define index      (and siblings            (index-of siblings active-menu-item)))
  (define index-new  (and index               (+ offset index)))
  (define active-new (and index-new           (list-ref siblings index-new)))
  (cond [(Menu-Item? active-new)
         (set-box! active-path-box (swap-last active-path
                                              (Menu-Item-tag active-new)))
         (Menu-Item-active-transition! active-menu-item active-new t)
         s]
        [else s]))
