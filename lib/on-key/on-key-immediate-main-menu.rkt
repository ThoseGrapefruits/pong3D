#lang typed/racket/base

(require racket/function
         "../menus/main-menu.rkt"
         "../state/state.rkt"
         "./util.rkt")

(provide on-key-immediate-main-menu)

(: on-key-immediate-main-menu : State-Main-Menu Natural Flonum String -> State)
(define (on-key-immediate-main-menu s n t k)
  (printf "on-key-immediate-main-menu: ~s~n" k)
  (: pressed? : String * -> Boolean)
  (define pressed? (curry (curry just-pressed? s) k))

  (cond
    [(pressed? "return" "enter" "space"
               (list->string '(#\return))
               (list->string '(#\newline))
               (list->string '(#\return #\newline))
               (list->string '(#\space)))
     (printf "go-in~n")
     (Main-Menu-go-in s n t)]
    [(pressed? "escape" (list->string '(#\b)))
     (printf "go-out~n")
     (Main-Menu-go-in s n t)]
    [(pressed? "up" "w")
     (printf "go-vertical ~s~n" -1)
     (Main-Menu-go-vertical s n t -1)]
    [(pressed? "down" "s")
     (printf "go-vertical ~s~n" 1)
     (Main-Menu-go-vertical s n t 1)]
    [else s]))
