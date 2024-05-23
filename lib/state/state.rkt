#lang typed/racket/base

(require pict3d
         "../util/pid.rkt")

(provide Bounds
         (struct-out Ball)
         (struct-out Menu-Item)
         (struct-out Opponent)
         (struct-out Player)
         (struct-out State)
         (struct-out State-Game-Over)
         (struct-out State-Main-Menu)
         (struct-out State-Paused)
         (struct-out State-Play))

; TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Bounds (Pairof (U #f Pos) (U #f Pos)))

; STRUCTS — CHILDREN / SHARED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Ball
  ([dir : Dir]
   [pos : Pos]))

(struct Menu-Item
  ([bounding-rectangle : (U #f Bounds)]
   [drawn : (U #f Pict3D)]))

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

(struct State
   ; Delta time from the last frame.
  ([dt               : Flonum]

   ; The most recent screenspace position of the mouse.
   [mouse-pos-last   : (Pairof Integer Integer)]

   ; The most recent mouse trace. Will be unset if the mouse stops hovering
   ; something raytraceable.
   [mouse-trace      : (U #f Surface-Data)]

   ; The last successful mouse trace. Will be held until a new, non-#f value for
   ; mouse-trace is set.
   [mouse-trace-last : (U #f Surface-Data)]

   ; The number of elapsed ticks
   [n                : Natural]

   ; The last rendered picture. Used for raytracing.
   [pict-last        : (Boxof Pict3D)]

   ; Set of all currently-pressed keys (mouse + keyboard).
   [pressed          : (Setof String)]

   ; Total elapsed time.
   [t                : Flonum]

   ; Dimensions of the render window, in pixels.
   [window-dims      : (Pairof Index Index)])
  #:transparent)

(struct State-Game-Over State
  ([end-state : State-Play])
  #:transparent)

(struct State-Main-Menu State
  ([ items : (Listof Menu-Item) ])
  #:transparent)

(struct State-Paused State
  ; hold a copy of the last play state for when the game resumes
  ([resume-state : State-Play])
  #:transparent)

(struct State-Play State
  ([ball : Ball]
   [ball-predicted-pos-ends : (Listof Pos)]
   [opponent : Opponent]
   [player : Player]
   [start-t : Flonum])
  #:transparent)
