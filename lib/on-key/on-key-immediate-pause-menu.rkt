#lang typed/racket/base

(require "./util.rkt"
         "../menus/menu-pause.rkt"
         "../state/state.rkt")

(provide on-key-immediate-pause-menu)

(: on-key-immediate-pause-menu : State-Pause-Menu Natural Flonum String -> State-Any)
(define (on-key-immediate-pause-menu s n t k)
  (cond
    [(just-pressed? s k "escape" "a")
     (Pause-Menu-go-out      s n t        )]
    [(just-pressed? s k "space"  "d" "\n" "\r")
     (Pause-Menu-go-in       s n t 'active)]
    [(just-pressed? s k "up"     "w")
     (Pause-Menu-go-vertical s n t      -1)]
    [(just-pressed? s k "down"   "s")
     (Pause-Menu-go-vertical s n t       1)]
    [else s]))
