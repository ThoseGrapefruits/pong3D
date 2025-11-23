#lang typed/racket/base

(require "../state/init.rkt"
         "../state/menu.rkt"
         "../state/state.rkt"
         "../state/syntax.rkt"
         "../state/updaters.rkt"
         "../util/tag.rkt"
         "./navigation.rkt")

(provide Pause-Menu-activate
         Pause-Menu-go-horizontal
         Pause-Menu-go-in
         Pause-Menu-go-out
         Pause-Menu-go-vertical)

(: Pause-Menu-activate : (Menu-On-Activate State-Pause-Menu))
(define (Pause-Menu-activate s n t path)
  (cond [(not (State-Pause-Menu? s)) s]
        [(path=? path (list 'root-pause 'resume))
         (define resume-state (State-Pause-Menu-resume-state s))
         (if (State-Pause-Menu-resume-state s)
             (State-update-counters resume-state n t)
             (error "pause state with no resume-state"))]
        [(path=? path (list 'root-pause 'main-menu))
         (state-start s t)]
        [(path=? path (list 'root-pause 'exit))
         (State-transition State-Stop s)]
        [else s]))

(: Pause-Menu-change : (Menu-On-Change State-Pause-Menu))
(define (Pause-Menu-change s n t path menu-item)
  (printf "Pause-Menu-change ~v ~v~n" path menu-item)
  (cond [(not (State-Pause-Menu? s)) s]
        [(path=? path (list 'root-main 'settings 'sound 'volume-main))
         ; TODO play demo sounds
         s]
        [else s]))

(: Pause-Menu-exit : (Menu-On-Exit State-Pause-Menu))
(define (Pause-Menu-exit s n t)
  (define resume-state (State-Pause-Menu-resume-state s))
  (when resume-state
    (box-cas! (State-t resume-state) (unbox (State-t resume-state)) t))
  (struct-copy
   State-Play resume-state
   [pause-state (struct-copy State-Pause-Menu s
                             ; clear the old resume-state, will
                             ; get reset if re-pause-menu
                             [resume-state #f])]))

(: Pause-Menu-go-out : State-Pause-Menu Natural Flonum -> State-Any)
(define (Pause-Menu-go-out s n t)
  (define menu (State-Pause-Menu-menu s))
  (Menu-go-out s menu n t Pause-Menu-exit))

(: Pause-Menu-go-in : State-Pause-Menu Natural Flonum Path-Source -> State-Any)
(define (Pause-Menu-go-in s n t path-source)
  (define menu (State-Pause-Menu-menu s))
  (Menu-go-in s menu n t path-source Pause-Menu-activate))

(: Pause-Menu-go-horizontal : State-Pause-Menu Natural Flonum (U -1 1) -> State-Any)
(define (Pause-Menu-go-horizontal s n t offset)
  (define menu (State-Pause-Menu-menu s))
  (Menu-go-horizontal s menu n t offset #:on-change Pause-Menu-change))

(: Pause-Menu-go-vertical : State-Pause-Menu Natural Flonum (U -1 1) -> State-Any)
(define (Pause-Menu-go-vertical s n t offset)
  (define menu (State-Pause-Menu-menu s))
  (Menu-go-vertical s menu n t offset))
