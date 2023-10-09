#lang typed/racket

(require "./state.rkt")

(provide (all-defined-out))

; [distant, maniacal laughter]
; https://stackoverflow.com/a/77208704/2883258
;
; This horrible macro expansion thing lets us get around the fact that
; struct-copy doesn't have much runtime flexibility with choosing what child
; struct to use when copying, while avoiding repeating the same cond statement
; in a bunch of different update methods.
(define-syntax-rule (State-update-parent s [t #:parent State v] ...)
  (cond [(State-Game-Over? s)
         (struct-copy State-Game-Over s [t #:parent State v] ...)]
        [(State-Main-Menu? s)
         (struct-copy State-Main-Menu s [t #:parent State v] ...)]
        [(State-Paused? s)
         (struct-copy State-Paused s [t #:parent State v] ...)]
        [(State-Play? s)
         (struct-copy State-Play s [t #:parent State v] ...)]
        [else (error (~s "Invalid state: " s))]))

; This other horrible macro syntax expansion thing lets us transition between
; states more easily, copying the fields stored on the base State struct.
(define-syntax-rule (State-transition S s field ...)
  (S (State-dt s)
     (State-n s)
     (State-pressed s)
     (State-t s)
     field ...))