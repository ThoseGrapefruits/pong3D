#lang typed/racket/base

(require racket/function
         "../menus/main-menu.rkt"
         "../state/state.rkt"
         "./util.rkt")

(provide on-key-immediate-main-menu)

(: on-key-immediate-main-menu : State-Main-Menu Natural Flonum String -> State-Any)
(define (on-key-immediate-main-menu s n t k)
  (: pressed? : String * -> Boolean)
  (define pressed? (curry (curry just-pressed? s) k))

  (cond
    [(pressed? "return" "enter" "space"
               (list->string '(#\return))
               (list->string '(#\newline))
               (list->string '(#\return #\newline))
               (list->string '(#\space)))
     (Main-Menu-go-in s n t 'active)]
    [(pressed? "escape" (list->string '(#\b)))
     (Main-Menu-go-out s n t)]
    [(pressed? "up" "w")
     (Main-Menu-go-vertical s n t -1)]
    [(pressed? "down" "s")
     (Main-Menu-go-vertical s n t 1)]
    [else s]))
