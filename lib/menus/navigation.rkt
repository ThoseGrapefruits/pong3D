#lang typed/racket/base

(require racket/bool
         racket/list
         "../state/menu.rkt"
         "../state/menu-item-types.rkt"
         "../state/state.rkt"
         "../util/list.rkt"
         "../util/tag.rkt")

(provide Menu-On-Activate
         Menu-On-Change
         Menu-On-Exit
         Menu-go-in
         Menu-go-horizontal
         Menu-go-out
         Menu-go-vertical)

(define-type (Menu-On-Activate S) (-> (∩ S State-Any) Natural Flonum Tags           State-Any))
(define-type (Menu-On-Change S)   (-> (∩ S State-Any) Natural Flonum Tags Menu-Item State-Any))
(define-type (Menu-On-Exit S)     (-> (∩ S State-Any) Natural Flonum                State-Any))

(: Menu-go-in : (All (S) (∩ S State-Any) Menu Natural Flonum Path-Source (Menu-On-Activate S)
                     -> State-Any))
(define (Menu-go-in s menu n t path-source on-activate)
  (define active-path-box
    (cond [(symbol=? path-source 'active) (Menu-active-path menu)]
          [(symbol=? path-source 'hover) (Menu-hovered-path menu)]
          [else (error 'Menu-go-in "unknown Path-Source ~s" path-source)]))

  (: active-path : (U #f Tags))
  (define active-path (unbox active-path-box))
  (define active-menu-item (and active-path (Menu-ref menu active-path)))
  (define active-children (and active-menu-item
                               (Menu-Item-children active-menu-item)))
  (define first-child (and active-children
                           (not (empty? active-children))
                           (first active-children)))
  (printf "Menu-go-in. active-path: ~s, active-menu-item: ~s~n"
                       active-path      active-menu-item)
  (cond [(and active-path
              (Menu-Item? first-child))
         (set-box! active-path-box (append active-path
                                           (list (Menu-Item-tag first-child))))
         (Menu-Item-active-transition! active-menu-item first-child t)
         s]
        [active-path (on-activate s n t active-path)]
        [else s]))

(: Menu-go-horizontal : (All (S) (∩ S State-Any) Menu Natural Flonum (U -1 1)
                             #:on-change (Menu-On-Change S) -> State-Any))
(define (Menu-go-horizontal s menu n t offset #:on-change on-change)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-mi (and active-path (Menu-ref menu active-path)))
  (define active-type (and active-mi (Menu-Item-type active-mi)))
  (cond [(Menu-Item-Type-Slider-Flonum? active-type)
         (define value-set! (Menu-Item-Type-Slider-Flonum-value-setter active-type))
         (value-set! (+ ((Menu-Item-Type-Slider-Flonum-value-getter  active-type))
                        (* offset (Menu-Item-Type-Slider-Flonum-step active-type))))
         (on-change s n t active-path active-mi)
         s]
        [else s]))

(: Menu-go-out : (All (S) (∩ S State-Any) Menu Natural Flonum (Menu-On-Exit S) -> State-Any))
(define (Menu-go-out s menu n t on-exit)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (and active-path (Menu-ref menu active-path)))
  (define parent (and active-menu-item (unbox (Menu-Item-parent active-menu-item))))
  (printf "Menu-go-out. parent: ~s"
                        parent    )
  (cond [(Menu-Item? parent)
         (set-box! active-path-box (drop-right active-path 1))
         (Menu-Item-active-transition! active-menu-item parent t)
         s]
        [(Menu? parent)
         (on-exit s n t)]
        [else (error 'Menu-go-out "weird parent: ~s" parent)]))

(: Menu-go-vertical : (All (S) (∩ S State-Any) Menu Natural Flonum (U -1 1) -> State-Any))
(define (Menu-go-vertical s menu n t offset)
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (and active-path (Menu-ref menu active-path)))
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
  (cond [(and active-path (Menu-Item? active-new))
         (set-box! active-path-box (if root?
                                       (append active-path
                                               (list (Menu-Item-tag active-new)))
                                       (swap-last active-path
                                                  (Menu-Item-tag active-new))))
         (Menu-Item-active-transition! active-menu-item active-new t)
         s]
        [else s]))
