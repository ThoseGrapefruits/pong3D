#lang typed/racket/base

(require (only-in "../state/init.rkt" state-start)
         (only-in "../state/menu.rkt" Path-Source)
         (only-in "../state/state.rkt"
                  State State-Any State-Pause-Menu State-Pause-Menu? State-Pause-Menu-menu State-Pause-Menu-resume-state State-Play State-Stop)
         (only-in "../state/syntax.rkt" State-transition)
         (only-in "../state/updaters.rkt" State-update-counters)
         (only-in "../util/tag.rkt" path=?)
         (only-in "./navigation.rkt"
                  Menu-On-Activate Menu-On-Change Menu-On-Exit
                  Menu-go-horizontal Menu-go-in Menu-go-out Menu-go-vertical))

(provide Pause-Menu-activate
         Pause-Menu-go-out
         Pause-Menu-go-in
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

(: Pause-Menu-exit : (Menu-On-Exit State-Pause-Menu))
(define (Pause-Menu-exit s n t)
  (struct-copy
   State-Play (State-Pause-Menu-resume-state s)
   [t #:parent State t]
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

(: Pause-Menu-go-vertical : State-Pause-Menu Natural Flonum (U -1 1) -> State-Any)
(define (Pause-Menu-go-vertical s n t offset)
  (define menu (State-Pause-Menu-menu s))
  (Menu-go-vertical s menu n t offset))
