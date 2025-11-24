#lang typed/racket/base

(require
  "./util.rkt"
  "../state/menu.rkt"
  "../state/menu-root-pause.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt")

(require/typed typed/racket [current-inexact-monotonic-milliseconds (-> Real)])

(provide on-key-immediate-play)

(: on-key-immediate-play : State-Play Natural Flonum String -> State-Any)
(define (on-key-immediate-play s n t k)
  (cond
    [(just-pressed? s k "escape")
     (define pause-state (State-Play-pause-state s))
     (define resume-state
       (struct-copy State-Play s  ; resume-state
                    [pause-state #f]))
     ; try to use cached state
     (or (and pause-state
              (struct-copy State-Pause-Menu pause-state
                           [resume-state resume-state]))
         ; fall back to making a new one
         (State-transition
          State-Pause-Menu s
          (make-Menu      ; menu
           (get-menu-root-pause))
          (current-inexact-monotonic-milliseconds)
          resume-state))] ; resume-state
    [else s]))
