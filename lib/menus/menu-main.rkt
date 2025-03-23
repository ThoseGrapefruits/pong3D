#lang typed/racket/base

(require "../state/init.rkt"
         "../state/menu.rkt"
         "../state/state.rkt"
         "../state/syntax.rkt"
         "../util/tag.rkt"
         "./navigation.rkt")

(provide Main-Menu-activate
         Main-Menu-go-in
         Main-Menu-go-horizontal
         Main-Menu-go-out
         Main-Menu-go-vertical)

(: Main-Menu-activate : (Menu-On-Activate State-Main-Menu))
(define (Main-Menu-activate s n t path)
  (printf "Main-Menu-activate: ~v~n" path)
  (cond [(not (State-Main-Menu? s)) s]
        [(path=? path (list 'root-main 'start))  (state-reset-play s n t)]
        [(path=? path (list 'root-main 'exit)) (State-transition State-Stop s)]
        [else s]))

(: Main-Menu-change : (Menu-On-Change State-Main-Menu))
(define (Main-Menu-change s n t path menu-item)
  (printf "Main-Menu-change ~v~n" path menu-item)
  (cond [(not (State-Main-Menu? s)) s]
        [(path=? path (list 'root-main 'settings 'sound 'volume-main))
         ; TODO play demo sounds
         s]
        [else s]))

(: Main-Menu-exit : (Menu-On-Exit State-Main-Menu))
(define (Main-Menu-exit s n t)
  ; do nothing. could prompt to exit in the future.
  s)

(: Main-Menu-go-out : State-Main-Menu Natural Flonum -> State-Any)
(define (Main-Menu-go-out s n t)
  (define menu (State-Main-Menu-menu s))
  (Menu-go-out s menu n t Main-Menu-exit))

(: Main-Menu-go-horizontal : State-Main-Menu Natural Flonum (U -1 1) -> State-Any)
(define (Main-Menu-go-horizontal s n t offset)
  (define menu (State-Main-Menu-menu s))
  (Menu-go-horizontal s menu n t offset #:on-change Main-Menu-change))

(: Main-Menu-go-in : State-Main-Menu Natural Flonum Path-Source -> State-Any)
(define (Main-Menu-go-in s n t path-source)
  (define menu (State-Main-Menu-menu s))
  (Menu-go-in s menu n t path-source Main-Menu-activate))

(: Main-Menu-go-vertical : State-Main-Menu Natural Flonum (U -1 1) -> State-Any)
(define (Main-Menu-go-vertical s n t offset)
  (define menu (State-Main-Menu-menu s))
  (Menu-go-vertical s menu n t offset))
