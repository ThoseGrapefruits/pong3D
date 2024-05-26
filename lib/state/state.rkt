#lang typed/racket/base

(require pict3d
         "./menu.rkt"
         "./state-base.rkt"
         "../util/pid.rkt")

(provide Bounds
         bounds?
         (struct-out Ball)
         (struct-out Menu)
         (struct-out Menu-Item)
         (struct-out Opponent)
         (struct-out Player)
         (struct-out State)
         (struct-out State-Game-Over)
         (struct-out State-Main-Menu)
         (struct-out State-Pause-Menu)
         (struct-out State-Play))

; TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Bounds (Pairof (U #f Pos) (U #f Pos)))

(: bounds? : Any -> Boolean : Bounds)
(define (bounds? o)
  (cond [(pair? o) (and (or (not  (car o))
                            (pos? (car o)))
                        (or (not  (cdr o))
                            (pos? (cdr o))))]
        [else      #f]))

; STRUCTS — CHILDREN / SHARED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Ball
  ([dir : Dir]
   [pos : Pos]))

(struct Opponent
  ([y : Flonum ]))

(struct Player
  ([lives : Nonnegative-Integer]
   [score : Nonnegative-Integer]
   [score-multiplier : Nonnegative-Flonum]
   [y : Flonum]
   [y-desired : (U Flonum #f)]
   [y-pid : PID]))

; STRUCTS — STATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct State-Game-Over State
  ([end-state : State-Play])
  #:transparent)

(struct State-Main-Menu State
  ([menu : Menu])
  #:transparent)

(struct State-Pause-Menu State
   ; The menu.
  ([menu : Menu]
   ; Copy of the last play state for when the game resumes. Only #f when this
   ; State-Pause-Menu is stored in pause-state of State-Play.
   [resume-state : (U #f State-Play)])
  #:transparent)

(struct State-Play State
  ([ball : Ball]
   [ball-predicted-pos-ends : (Listof Pos)]
   [opponent : Opponent]
   ; Hold a copy of the last play state for if the game is paused again. Mostly
   ; an optimization but also might let us do stuff we wouldn't otherwise be
   ; able to do.
   [pause-state : (U #f State-Pause-Menu)]
   [player : Player]
   [start-t : Flonum])
  #:transparent)
