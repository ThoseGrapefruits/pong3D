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
    on-frame-play-ball-position
    on-frame-play-ball-direction) s))

; TODO this stuff still isn't quite right, and gets more incorrect the faster
; the ball is moving + the more oblique its trajectory is. We basically need to
; calculate where the ball would hit on the paddle, then move it there and then
; reflected back out proportionally to how far away it is from the paddle. Right
; now, I'm half-doing this, but the collision point is the ball's current y,
; instead of the calculated y collision point. Probably not a huge difference in
; most circumstances but it's still wrong and probably makes some collisions
; feel bad / wrong.

(: on-frame-play-ball-collision-bumper : State-Play -> State-Play)
(define (on-frame-play-ball-collision-bumper s)
  (define player (State-Play-player s))
  (define opponent (State-Play-opponent s))
  (define ball (State-Play-ball s))
  (define ball-speed-x (abs (dir-dx (Ball-dir ball))))
  (define ball-berth-x (* ball-speed-x (* (State-dt s) BALL-SPEED)))
  (cond
    ; opponent collision
    [(and (negative? (dir-dx (Ball-dir ball)))
          (within? (pos-x (Ball-pos ball))
                   (- OPPONENT-X CONTACT-BUFFER ball-berth-x)
                   OPPONENT-X-COLLISION)
          (within? (State-get-contact-offset-y s (Opponent-y opponent))
                   (- (dir-dy BUMPER-SCALE))
                   (dir-dy BUMPER-SCALE)))
     (define reflection-yaw
       (* (- 180 REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Opponent-y opponent))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define ball-new (struct-copy
             Ball ball
             [dir (dir-scale (dir-reflect (Ball-dir ball) reflection-axis)
                             BALL-ACCELERATION-PADDLE)]))
     (rs-play-random SOUNDS-BALL-BOUNCE-BUMPER)
     (struct-copy
      State-Play s
      [ball ball-new]
      [ball-predicted-pos-ends (predict-ball-pos-ends-2 ball-new)])]
    ; player collision
    [(and (positive? (dir-dx (Ball-dir ball)))
          (within? (pos-x (Ball-pos ball))
                   PLAYER-X-COLLISION
                   (+ PLAYER-X CONTACT-BUFFER ball-berth-x))
          (within? (State-get-contact-offset-y s (Player-y player))
                   (- (dir-dy BUMPER-SCALE))
                   (dir-dy BUMPER-SCALE)))
     (define reflection-yaw
       (* (- REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Player-y player))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define ball-new
       (struct-copy
        Ball ball
        [dir (dir-scale (dir-reflect (Ball-dir ball) reflection-axis)
                        BALL-ACCELERATION-PADDLE)]))
     (rs-play-random SOUNDS-BALL-BOUNCE-BUMPER)
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
  (cond
    ; left wall collision
    [(and (negative? (dir-dy (Ball-dir ball)))
          (< (pos-y (Ball-pos ball)) (- BALL-MAX-Y)))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (struct-copy
      State-Play s
      [ball (struct-copy
             Ball ball
             [dir (dir-scale
                   (dir-reflect (Ball-dir ball) +y)
                   BALL-ACCELERATION-WALL)])])]
    ; right wall collision
    [(and (positive? (dir-dy (Ball-dir ball)))
          (> (pos-y (Ball-pos ball)) BALL-MAX-Y))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (struct-copy
      State-Play s
      [ball (struct-copy
             Ball ball
             [dir (dir-scale
                   (dir-reflect (Ball-dir ball) +y)
                   BALL-ACCELERATION-WALL)])])]
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
