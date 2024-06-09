#lang typed/racket/base

(require
  "./util.rkt"
  "../state/init.rkt"
  "../state/state.rkt")

(provide on-key-immediate-game-over)

(: on-key-immediate-game-over : State-Game-Over Natural Flonum String -> State-Any)
(define (on-key-immediate-game-over s n t k)
  (cond
    [(just-pressed? s k "r") (state-reset-play s n t)]
    [else s]))
