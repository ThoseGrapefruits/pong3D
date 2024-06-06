#lang typed/racket/base

(require racket/list
         "../state/init.rkt"
         "../state/menu.rkt"
         "../state/state.rkt"
         "../state/syntax.rkt"
         "../util/list.rkt"
         "../util/number.rkt"
         "../util/tag.rkt")

(provide Main-Menu-activate
         Main-Menu-go-out
         Main-Menu-go-in
         Main-Menu-go-vertical)

(: Main-Menu-activate : State-Main-Menu Natural Flonum Tags -> State-Any)
(define (Main-Menu-activate s n t path)
  (cond [(path=? path (list 'root-main 'start))  (state-reset-play s n t)]
        [(path=? path (list 'root-main 'exit)) (State-transition State-Stop s)]
        [else s]))

(: Main-Menu-go-out : State-Main-Menu Natural Flonum -> (U State-Main-Menu State-Stop))
(define (Main-Menu-go-out s n t)
  (define menu (State-Main-Menu-menu s))
  (define active-path-box (Menu-active-path menu))
  (define active-path (unbox active-path-box))
  (define active-menu-item (Menu-ref menu active-path))
  (define parent (and active-menu-item (Menu-Item-parent active-menu-item)))
  (cond [(Menu-Item? parent)
         (set-box! active-path-box (drop-right active-path 1))
         (Menu-Item-active-transition! active-menu-item parent t)
         s]
        [(Menu? parent) (State-transition State-Stop s)]
        [else s]))

(: Main-Menu-go-in : State-Main-Menu Natural Flonum -> State-Any)
(define (Main-Menu-go-in s n t)
  (define menu (State-Main-Menu-menu s))
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
        [else (Main-Menu-activate s n t active-path)]))

(: Main-Menu-go-vertical : State-Main-Menu Natural Flonum (U -1 1) -> State-Main-Menu)
(define (Main-Menu-go-vertical s n t offset)
  (define menu (State-Main-Menu-menu s))
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
  (printf "active-path: ~s~nactive-menu-item: ~s~nparent: ~s~nsiblings: ~s index:~s~n~n"
           active-path      active-menu-item      parent      siblings     index)
  (cond [(Menu-Item? active-new)
         (set-box! active-path-box (if root?
                                       (append active-path
                                               (list (Menu-Item-tag active-new)))
                                       (swap-last active-path
                                                  (Menu-Item-tag active-new))))
         (Menu-Item-active-transition! active-menu-item active-new t)
         s]
        [else s]))

