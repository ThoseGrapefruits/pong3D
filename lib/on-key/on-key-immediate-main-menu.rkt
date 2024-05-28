#lang typed/racket/base

(require "../menus/main-menu.rkt"
         "../state/state.rkt"
         "./util.rkt")

(provide on-key-immediate-main-menu)

(: on-key-immediate-main-menu : State-Main-Menu Natural Flonum String -> State)
(define (on-key-immediate-main-menu s n t k)
  (cond
    [(just-pressed? s k "return" "enter" "\n" "\r")
     (Main-Menu-go-in s n t)]
    [else s]))
