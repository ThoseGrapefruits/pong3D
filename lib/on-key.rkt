#lang typed/racket/base

(require
  "./state/init.rkt"
  "./state/setters.rkt"
  "./state/state.rkt")

(provide on-key
         on-release)

(: on-key : State Natural Flonum String -> State)
(define (on-key s n t k)
  (State-set-key-pressed
   (on-key-immediate s n t k)
   k #t))

(: on-key-immediate : State Natural Flonum String -> State)
; Immediate reactions to keypresses
(define (on-key-immediate s n t k)
  (cond
    [(string=? k "Escape")
     (cond [(State-Play? s) (struct-copy State-Paused s)]
           [(State-Paused? s) (State-Paused-resume-state s)]
           [else s])]
    [(string=? k "r")
     (state-reset s n t)]
    [else s]))

(: on-release : State Natural Flonum String -> State)
(define (on-release s n t k)
  (State-set-key-pressed s k #f))