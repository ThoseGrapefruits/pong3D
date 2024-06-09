#lang typed/racket/base

(require racket/list
         "../state/menu.rkt"
         "../state/state.rkt"
         "../state/syntax.rkt"
         "../util/list.rkt"
         "../util/tag.rkt")

(provide Menu-On-Activate
         Menu-On-Exit
         Menu-go-in
         Menu-go-out
         Menu-go-vertical)

(define-type (Menu-On-Activate S) (-> (∩ S State-Any) Natural Flonum Tags State-Any))
(define-type (Menu-On-Exit S)     (-> (∩ S State-Any) Natural Flonum      State-Any))

(: Menu-go-in : (All (S) (∩ S State-Any) Menu Natural Flonum (Menu-On-Activate S) -> State-Any))
(define (Menu-go-in s menu n t on-activate)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define active-children (and active-menu-item
                               (Menu-Item-children active-menu-item)))
  (define first-child (and active-children
                           (not (empty? active-children))
                           (first active-children)))
  (define first-child-is-terminal (and first-child
                                       (empty? (Menu-Item-children first-child))))
  (cond [(and (Menu-Item? first-child)
              (not first-child-is-terminal))
         (set-box! active-path-box (append active-path
                                           (list (Menu-Item-tag first-child))))
         (Menu-Item-active-transition! active-menu-item first-child t)
         s]
        [else (on-activate s n t active-path)]))

(: Menu-go-out : (All (S) (∩ S State-Any) Menu Natural Flonum (Menu-On-Exit S) -> State-Any))
(define (Menu-go-out s menu n t on-exit)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define parent (and active-menu-item (Menu-Item-parent active-menu-item)))
  (cond [(Menu-Item? parent)
         (set-box! active-path-box (drop-right active-path 1))
         (Menu-Item-active-transition! active-menu-item parent t)
         s]
        [(Menu? parent) (State-transition State-Stop s)]
        [else (on-exit s n t)]))

(: Menu-go-vertical : State-Any Menu Natural Flonum (U -1 1) -> State-Any)
(define (Menu-go-vertical s menu n t offset)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define parent     (and active-menu-item    (unbox (Menu-Item-parent active-menu-item))))
  (define root? (Menu? parent))
  (define siblings   (cond [(Menu-Item? parent) (Menu-Item-children parent)]
                           [root?               (Menu-Item-children active-menu-item)]
                           [else #f]))
  (define index      (cond [(not siblings) #f]
                           [root?          0]
                           [else           (index-of siblings active-menu-item)]))
  (define index-new  (and index (+ offset index)))
  (define index-new-wrapped (cond [(not index-new) #f]
                                  [(< index-new 0) (- (length siblings) 1)]
                                  [(>= index-new (length siblings)) 0]
                                  [else index-new]))
  (define active-new (and index-new-wrapped (list-ref siblings index-new-wrapped)))
  (cond [(Menu-Item? active-new)
         (set-box! active-path-box (if root?
                                       (append active-path
                                               (list (Menu-Item-tag active-new)))
                                       (swap-last active-path
                                                  (Menu-Item-tag active-new))))
         (Menu-Item-active-transition! active-menu-item active-new t)
         s]
        [else s]))
