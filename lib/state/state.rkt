#lang typed/racket/base

(require pict3d)

(provide
 (struct-out Ball)
 (struct-out Opponent)
 (struct-out Player)
 (struct-out State)
 (struct-out State-Game-Over)
 (struct-out State-Main-Menu)
 (struct-out State-Paused)
 (struct-out State-Play))

(struct Ball
  ([dir : Dir]
   [pos : Pos]))

(struct Opponent
  ([predicted-ball-y : (U Null Flonum)]
   [y : Flonum ]))

(struct Player
  ([lives : Nonnegative-Integer]
   [score : Nonnegative-Integer]
   [score-multiplier : Nonnegative-Flonum]
   [y : Flonum]))

(struct State
  ([dt   : Flonum]
   [n    : Natural]
   [pressed : (Setof String)]
   [t    : Flonum])
  #:transparent)

(struct State-Game-Over State
  ([end-state : State-Play])
  #:transparent)

(struct State-Main-Menu State
  ()
  #:transparent)

(struct State-Paused State
  ; hold a copy of the last play state for when the game resumes
  ([resume-state : State-Play])
  #:transparent)

(struct State-Play State
  ([ball : Ball]
   [ball-predicted-pos : (U Pos Null)]
   [opponent : Opponent]
   [player : Player]
   [start-t : Flonum])
  #:transparent)
