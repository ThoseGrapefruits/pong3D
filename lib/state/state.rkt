#lang typed/racket/base

(require pict3d
         racket/match
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

(struct Menu
   ; The active/focused menu item. Defaults to Menu-root.
  ([active-item : Menu-Item]
   ; The root menu item, whose parent is this Menu
   [root : Menu-Item]))

(struct Menu-Item
   ; The direct descendents of this Menu-Item in the tree.
  ([children : (Listof Menu-Item)]
   ; The direct descendants of this Menu-Item, organized by tag for fast access.
   [children-map : (Weak-HashTable Tag Menu-Item)]
   [draw : (-> Pict3D)]
   ; The text label displayed on this menu item.
   [label : String]
   ; The direct parent of this Menu-Item in the tree. If it is a Menu, this
   ; Menu-Item is the root of a Menu.
   [parent : (U Menu Menu-Item)]
   ; The tag for this Menu-Item. Used to look up menu items from raytraces.
   [tag : Tag]))

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
  ([menu : Menu])
  #:transparent)

(struct State-Pause-Menu State
  ; hold a copy of the last play state for when the game resumes. Only #f when
  ; this State-Pause-Menu is stored in pause-state of State-Play.
  ([menu : Menu]
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
