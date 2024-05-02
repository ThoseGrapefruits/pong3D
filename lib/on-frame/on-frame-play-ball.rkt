#lang typed/racket

(require
  pict3d
  "../config.rkt"
  "../sound/index.rkt"
  "../state/accessors.rkt"
  "../state/state.rkt"
  "../util/ball/ball-prediction.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt")

(provide on-frame-play-ball)

(: on-frame-play-ball : State-Play -> State-Play)
(define (on-frame-play-ball s)
  ((compose1
    on-frame-play-ball-direction
    on-frame-play-ball-position) s))

; TODO this stuff still isn't quite right, and gets more incorrect the faster
; the ball is moving, the more oblique its trajectory, and the lower the
; framerate. I'll need to start doing something like:
;   1. on-frame-play-ball-position-pre: If the ball would reflect this frame, have 
;      move the ball to its exact reflection location with some flag set in
;      state to indicate its reflection axis + the proportion of dt consumed by
;      the move to the reflection location.
;   2. on-frame-play-ball-direction: If the ball is reflecting, based on flag
;      from step 1, set the relevant direction based on the set direction.
;   3. on-frame-play-ball-position-post: If the ball is reflecting, based on
;      flag from step 1, do another positional update along the reflection axis
;      based on the remaining dt.

(define BUMPER-SCALE-Y (dir-dy BUMPER-SCALE))

(: on-frame-play-ball-collision-bumper : State-Play -> State-Play)
(define (on-frame-play-ball-collision-bumper s)
  (define player (State-Play-player s))
  (define opponent (State-Play-opponent s))
  (define ball (State-Play-ball s))
  (define ball-x (pos-x (Ball-pos ball)))

  (cond
    ; opponent collision
    [(and (negative? (dir-dx (Ball-dir ball)))
          (< ball-x OPPONENT-X-COLLISION)
          (within? (State-get-contact-offset-y s (Opponent-y opponent))
                   (- BUMPER-SCALE-Y)
                   BUMPER-SCALE-Y))
     (define reflection-yaw
       (* (- 180 REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Opponent-y opponent))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define dist-past-bumper (- OPPONENT-X-COLLISION ball-x))
     (define ball-new
       (struct-copy
        Ball ball
        [dir (dir-scale (dir-reflect (Ball-dir ball) reflection-axis)
                        BALL-ACCELERATION-PADDLE)]
        ; TODO this should take into account the reflection-axis
        [pos (pos+ (Ball-pos ball) +x (* 2.0 dist-past-bumper))]))
     (rs-play-random SOUNDS-BALL-BOUNCE-OPPONENT)
     (struct-copy
      State-Play s
      [ball ball-new]
      [ball-predicted-pos-ends (predict-ball-pos-ends-2 ball-new)])]

    ; player collision
    [(and (positive? (dir-dx (Ball-dir ball)))
          (> ball-x PLAYER-X-COLLISION)
          (within? (State-get-contact-offset-y s (Player-y player))
                   (- BUMPER-SCALE-Y)
                   BUMPER-SCALE-Y))
     (define reflection-yaw
       (* (- REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Player-y player))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define dist-past-bumper (- ball-x PLAYER-X-COLLISION))
     (define ball-new
       (struct-copy
        Ball ball
        [dir (dir-scale (dir-reflect (Ball-dir ball) reflection-axis)
                        BALL-ACCELERATION-PADDLE)]
        ; TODO this should take into account the reflection-axis
        [pos (pos+ (Ball-pos ball) -x (* 2.0 dist-past-bumper))]))
     (rs-play-random SOUNDS-BALL-BOUNCE-PLAYER)
     (struct-copy
      State-Play s
      [ball ball-new]
      [ball-predicted-pos-ends (predict-ball-pos-ends-2 ball-new)]
      [player
       (struct-copy
        Player player
        [score (get-new-player-score player 1)])])]

    [else s]))

(: on-frame-play-ball-collision-wall : State-Play -> State-Play)
(define (on-frame-play-ball-collision-wall s)
  (define ball (State-Play-ball s))
  (define ball-y (pos-y (Ball-pos ball)))
  (cond
    ; left wall collision
    [(and (negative? (dir-dy (Ball-dir ball)))
          (< ball-y (- BALL-MAX-Y)))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (define dist-past-wall (- (- BALL-MAX-Y) ball-y))
     (displayln dist-past-wall)
     (struct-copy
      State-Play s
      [ball (struct-copy
             Ball ball
             [dir (dir-scale
                   (dir-reflect (Ball-dir ball) +y)
                   BALL-ACCELERATION-WALL)]
             [pos (pos+ (Ball-pos ball) +y dist-past-wall)])])]

    ; right wall collision
    [(and (positive? (dir-dy (Ball-dir ball)))
          (> ball-y BALL-MAX-Y))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (define dist-past-wall (- ball-y BALL-MAX-Y))
     (displayln dist-past-wall)
     (struct-copy
      State-Play s
      [ball (struct-copy
             Ball ball
             [dir (dir-scale
                   (dir-reflect (Ball-dir ball) +y)
                   BALL-ACCELERATION-WALL)]
             [pos (pos+ (Ball-pos ball) -y dist-past-wall)])])]

    [else s]))

(: on-frame-play-ball-direction : State-Play -> State-Play)
(define (on-frame-play-ball-direction s)
  ((compose1
    on-frame-play-ball-collision-bumper
    on-frame-play-ball-collision-wall) s))

(: on-frame-play-ball-position : State-Play -> State-Play)
(define (on-frame-play-ball-position s)
  (define ball (State-Play-ball s))
  (struct-copy
   State-Play s
   [ball (struct-copy
          Ball ball
          [pos
           (pos+ (Ball-pos ball)
                 (dir-scale (Ball-dir ball)
                            (* (State-dt s) BALL-SPEED)))])]))
