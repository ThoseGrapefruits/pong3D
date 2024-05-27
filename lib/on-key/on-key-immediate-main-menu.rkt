#lang typed/racket/base

(require
  pict3d
  racket/list
  "../state/init.rkt"
  "../state/state.rkt"
  "../util/ball.rkt"
  "../util/number.rkt"
  "../util/pid.rkt"
  "./util.rkt")

(provide on-key-immediate-main-menu)

(: on-key-immediate-main-menu : State-Main-Menu Natural Flonum String -> State)
(define (on-key-immediate-main-menu s n t k)
  (cond
    [(just-pressed? s k "return" "enter" "\n" "\r")
     (state-reset-play s n t)]   ; start-t
    [else s]))
