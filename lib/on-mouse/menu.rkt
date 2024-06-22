#lang typed/racket/base

(require racket/bool
         racket/list
         pict3d
         "../menus/navigation.rkt"
         "../state/menu.rkt"
         "../state/menu-item-types.rkt"
         "../state/state.rkt"
         "../util/tag.rkt")

(provide Menu-on-mouse-left-up
         Menu-on-mouse-move)

(: on-drag-default (All (S) (Menu-On-Drag S)))
(define (on-drag-default s n t paths e) s)

(: Menu-on-mouse-left-down : (All (S) (∩ S State-Any) Menu Natural Flonum
                                  [#:on-drag (Menu-On-Drag S)]
                                  -> State-Any))
(define (Menu-on-mouse-left-down s menu n t #:on-drag [on-drag on-drag-default])
  (define trace      (State-trace-mouse s))
  (define trace-path (and trace (surface-data-path trace)))
  (define trace-item (and trace-path (Menu-ref menu trace-path)))
  (and (Menu-Item? trace-item)
       (Menu-Item-Type-Slider? (Menu-Item-type trace-item))
       (on-drag s n t trace-path 'start)
       (set-box! (Menu-dragged-path menu) trace-path))
  s)

;; TODO
; - finish drag stuff
; - figure out how the mouse position will actually be calculated to get the
;   new drag value
; - keyboard interaction
; - rendering

(: Menu-on-mouse-left-up : (All (S) (∩ S State-Any) Menu Natural Flonum (Menu-On-Activate S) -> State-Any))
(define (Menu-on-mouse-left-up s menu n t on-activate)
  (define mouse-up      (State-trace-mouse s))
  (define trace         (State-trace-mouse      s))
  (define mouse-up-path (and mouse-down (surface-data-path mouse-down)))
  (define trace-path      (and trace      (surface-data-path trace)))

  (printf "Menu-on-mouse-left-up: mouse-down-path: ~s, trace-path: ~s~n"
                                  mouse-down-path      trace-path)

  (cond [(and drag-path
              trace-path
              (not (empty? trace-path))
              (path=? drag-path trace-path))
]
  [(and mouse-down-path
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
