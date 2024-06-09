#lang typed/racket/base

(require
  typed-compose
  "./on-key-immediate-game-over.rkt"
  "./on-key-immediate-main-menu.rkt"
  "./on-key-immediate-pause-menu.rkt"
  "./on-key-immediate-play.rkt"
  "./util.rkt"
  "../state/init.rkt"
  "../state/setters.rkt"
  "../state/state.rkt")

(provide on-key
         on-release)

(: on-key : State-Any Natural Flonum String -> State-Any)
(define (on-key s n t k)
  ((compose-n ; bottom-to-top
    (λ ([s : State-Any]) : State-Any (State-set-key-pressed s k #t))
    (λ ([s : State-Any]) : State-Any (on-key-immediate s n t k))) s))

(: on-key-immediate : State-Any Natural Flonum String -> State-Any)
; Immediate reactions to keypresses
(define (on-key-immediate s n t k)
  (cond
    [(State-Play? s)       (on-key-immediate-play       s n t k)]
    [(State-Game-Over? s)  (on-key-immediate-game-over  s n t k)]
    [(State-Main-Menu? s)  (on-key-immediate-main-menu  s n t k)]
    [(State-Pause-Menu? s) (on-key-immediate-pause-menu s n t k)]
    [else s]))

(: on-release : State-Any Natural Flonum String -> State-Any)
(define (on-release s n t k)
  (State-set-key-pressed s k #f))
