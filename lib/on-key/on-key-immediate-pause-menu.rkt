#lang typed/racket/base

(require (only-in racket/function curry)
         "./util.rkt"
         "../menus/menu-pause.rkt"
         "../state/state.rkt")

(provide on-key-immediate-pause-menu)

(: on-key-immediate-pause-menu : State-Pause-Menu Natural Flonum String -> State-Any)
(define (on-key-immediate-pause-menu s n t k)
  (define pressed? (curry (curry just-pressed? s) k))

  (cond
    [(pressed? "escape" "a")
     (Pause-Menu-go-out      s n t)]
    [(pressed? "return" "enter" "space"
               (list->string '(#\return))
               (list->string '(#\newline))
               (list->string '(#\return #\newline))
               (list->string '(#\space)))
     (Pause-Menu-go-in       s n t 'active)]
    [(pressed? "up"     "w")
     (Pause-Menu-go-vertical s n t -1)]
    [(pressed? "down"   "s")
     (Pause-Menu-go-vertical s n t  1)]
    [(pressed? "left" "a")
     (Pause-Menu-go-horizontal s n t -1)]
    [(pressed? "right" "d")
     (Pause-Menu-go-horizontal s n t  1)]
    [else s]))
