#lang typed/racket/base

(require racket/format
         "./state.rkt")

(provide State-transition
         State-update-parent)

; This horrible macro syntax expansion thing lets us transition between
; states more easily, copying the fields stored on the base State struct.
(define-syntax-rule (State-transition S s field ...)
  (S (State-dt s)
     (State-mouse-pos-last s)
     (State-n s)
     (State-pict-last s)
     (State-pressed s)
     (State-t s)
     (State-trace-mouse s)
     (State-trace-mouse/last s)
     (State-trace-mouse-down s)
     (State-window-dims s)
     field ...))

; This horrible macro expansion thing lets us get around the fact that
; struct-copy doesn't have much runtime flexibility with choosing what child
; struct to use when copying, while avoiding repeating the same cond statement
; in a bunch of different update methods. This is only necessary when trying to
; update a generic State-typed instance — if you know the specific type, you can
; just use struct-copy.
; https://stackoverflow.com/a/77208704/2883258
(define-syntax-rule (State-update-parent s [t #:parent State v] ...)
  (cond [(State-Play? s)
         (struct-copy State-Play       s [t #:parent State v] ...)]
        [(State-Game-Over? s)
         (struct-copy State-Game-Over  s [t #:parent State v] ...)]
        [(State-Main-Menu? s)
         (struct-copy State-Main-Menu  s [t #:parent State v] ...)]
        [(State-Pause-Menu? s)
         (struct-copy State-Pause-Menu s [t #:parent State v] ...)]
        [(State-Stop? s)
         (struct-copy State-Stop       s [t #:parent State v] ...)]
        [else (error 'State-update-parent "invalid state ~s" s)]))
