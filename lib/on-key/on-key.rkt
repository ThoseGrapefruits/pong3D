#lang typed/racket/base

(require
  racket/set
  typed-compose
  "./on-key-immediate-pause-menu.rkt"
  "./util.rkt"
  "../state/init.rkt"
  "../state/setters.rkt"
  "../state/state.rkt")

(provide on-key
         on-release)

(: on-key : State Natural Flonum String -> State)
(define (on-key s n t k)
  ((compose-n ; bottom-to-top
    (λ ([s : State]) (State-set-key-pressed s k #t))
    (λ ([s : State]) (on-key-immediate s n t k))) s))

(: on-key-immediate : State Natural Flonum String -> State)
; Immediate reactions to keypresses
(define (on-key-immediate s n t k)
  (cond
    [(State-Play? s)       (on-key-immediate-play       s n t k)]
    [(State-Pause-Menu? s) (on-key-immediate-pause-menu s n t k)]
    [(State-Game-Over? s)  (on-key-immediate-game-over  s n t k)]
    [else s]))

(: on-key-immediate-game-over : State-Game-Over Natural Flonum String -> State)
(define (on-key-immediate-game-over s n t k)
  (cond
    [(just-pressed? s k "r") (state-reset s n t)]
    [else s]))

(: on-key-immediate-play : State-Play Natural Flonum String -> State)
(define (on-key-immediate-play s n t k)
  (cond
    [(just-pressed? s k "escape") (struct-copy State-Pause-Menu s)]
    [else s]))

(: on-release : State Natural Flonum String -> State)
(define (on-release s n t k)
  (State-set-key-pressed s k #f))
