#lang typed/racket/base

(require
  "./util.rkt"
  "../state/menu.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt")

(provide on-key-immediate-play)

(: on-key-immediate-play : State-Play Natural Flonum String -> State)
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
          (make-Menu (make-Menu-Item ; menu
                      #:children '()
                      #:label "Pause"
                      #:tag 'root-pause))
          resume-state))]            ; resume-state
    [else s]))