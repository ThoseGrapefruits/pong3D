#lang typed/racket/base

(require racket/list
         pict3d
         "../menus/navigation.rkt"
         "../state/menu.rkt"
         "../state/state.rkt"
         "../util/tag.rkt")

(provide Menu-on-mouse-left-down
         Menu-on-mouse-left-up
         Menu-on-mouse-move)

(: Menu-on-mouse-left-down : (All (S) (∩ S State-Any) Menu Natural Flonum -> State-Any))
(define (Menu-on-mouse-left-down s menu n t)
  (define trace      (State-trace-mouse s))
  (define trace-path (and trace (surface-data-path trace)))
  (define trace-item (and trace-path (Menu-ref menu trace-path)))
  s)

(: Menu-on-mouse-left-up : (All (S) (∩ S State-Any) Menu Natural Flonum
                                #:on-activate (Menu-On-Activate S)
                                -> State-Any))
(define (Menu-on-mouse-left-up s menu n t #:on-activate on-activate)
  (define mouse-down      (State-trace-mouse-down s))
  (define trace           (State-trace-mouse      s))
  (define mouse-down-path (and mouse-down (surface-data-path mouse-down)))
  (define trace-path      (and trace      (surface-data-path trace)))

  (cond [(and mouse-down-path
              trace-path
              (not (empty? trace-path))
              (path=? mouse-down-path trace-path))
         (set-box! (Menu-active-path menu) trace-path)
         (set-box! (Menu-hovered-path menu) trace-path)
         (on-activate s n t trace-path)]
        [else s]))

(: Menu-on-mouse-move : (All (S) (∩ S State-Any) Menu Natural Flonum -> State-Any))
(define (Menu-on-mouse-move s menu n t)
  (define hover-path      (unbox (Menu-hovered-path menu)))
  (define hover-item      (and hover-path (Menu-ref menu hover-path)))
  (define trace           (State-trace-mouse      s))
  (define trace-path      (and trace (surface-data-path trace)))
  (define trace-item      (and trace-path (Menu-ref menu trace-path)))

  (cond [(and trace-path
              trace-item
              (or (not hover-path)
                  (not (path=? hover-path trace-path))))
         (set-box! (Menu-hovered-path menu) trace-path)
         (Menu-Item-active-transition! hover-item trace-item t)
         s]
        [else s]))
