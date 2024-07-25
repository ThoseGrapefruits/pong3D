#lang typed/racket/base

(require
  pict3d
  typed-compose
  "../config.rkt"
  "../sound/sound.rkt"
  "../state/accessors.rkt"
  "../state/state.rkt"
  "../util/ball.rkt"
  "../util/number.rkt"
  "../util/player.rkt")

(provide on-frame-play-ball)

(: on-frame-play-ball : State-Play -> State-Play)
(define (on-frame-play-ball s)
  ((compose-n ; bottom-to-top
    on-frame-play-ball-direction
    on-frame-play-ball-position) s))

(define BUMPER-SCALE-Y (dir-dy BUMPER-SCALE))

(: on-frame-play-ball-direction : State-Play -> State-Play)
(define (on-frame-play-ball-direction s)
  (move-reflect s))

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

; Given the current state and a predicted future ball position, if that position
; is past a bound that should have reflected the ball, return the reflection
; axis and the current state of the ball
(: get-bound-passed : State-Play Pos ->
   (Values (U Dir #f) ; canonical axis w/o messing with paddle offset
           (U Dir #f) ; reflection axis (includes paddle offset)
           Flonum))   ; how much ball travel distance remains
(define (get-bound-passed s ball-pos-next)
  (define player (State-Play-player s))
  (define opponent (State-Play-opponent s))
  (define ball (State-Play-ball s))
  (define ball-dir (Ball-dir ball))
  (define ball-pos (Ball-pos ball))
  (define ball-x (pos-x ball-pos-next))
  (define ball-y (pos-y ball-pos-next))
  (define ball-moved (pos- ball-pos-next ball-pos))

  (cond 
    ; left wall collision
    [(and (negative? (dir-dy (Ball-dir ball)))
          (< ball-y (- BALL-MAX-Y)))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (define dist-past-wall-y (- (- BALL-MAX-Y) ball-y))
     (define dist-past-wall (* (pos-dist ball-pos ball-pos-next)
                               (/ dist-past-wall-y (abs (dir-dy ball-moved)))))
     (values +y +y dist-past-wall)]

    ; right wall collision
    [(and (positive? (dir-dy ball-dir))
          (> ball-y BALL-MAX-Y))
     (rs-play-random SOUNDS-BALL-BOUNCE-WALL)
     (define dist-past-wall-y (- ball-y BALL-MAX-Y))
     (define dist-past-wall (* (pos-dist ball-pos ball-pos-next)
                               (/ dist-past-wall-y (abs (dir-dy ball-moved)))))
     (values -y -y dist-past-wall)]

    ; opponent collision
    [(and (negative? (dir-dx ball-dir))
          (< ball-x OPPONENT-X-COLLISION)
          (within? (State-get-contact-offset-y s (Opponent-y opponent))
                   (- BUMPER-SCALE-Y)
                   BUMPER-SCALE-Y))
     (define reflection-yaw
       (* (- 180 REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Opponent-y opponent))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define dist-past-bumper-x (- OPPONENT-X-COLLISION ball-x))
     (define dist-past-bumper (* (pos-dist ball-pos ball-pos-next)
                               (/ dist-past-bumper-x (abs (dir-dx ball-moved)))))
     (rs-play-random SOUNDS-BALL-BOUNCE-OPPONENT)
     (values +x reflection-axis dist-past-bumper)]

    ; player collision
    [(and (positive? (dir-dx ball-dir))
          (> ball-x PLAYER-X-COLLISION)
          (within? (State-get-contact-offset-y s (Player-y player))
                   (- BUMPER-SCALE-Y)
                   BUMPER-SCALE-Y))
     (define reflection-yaw
       (* (- REDIRECT-FACTOR)
          (State-get-contact-offset-y s (Player-y player))))
     (define reflection-axis (angles->dir reflection-yaw 0))
     (define dist-past-bumper-x (- ball-x PLAYER-X-COLLISION))
     (define dist-past-bumper (* (pos-dist ball-pos ball-pos-next)
                               (/ dist-past-bumper-x (abs (dir-dx ball-moved)))))
     (rs-play-random SOUNDS-BALL-BOUNCE-PLAYER)
     (values -x reflection-axis dist-past-bumper)]
    [else (values #f #f 0.0)]))

(: move-reflect : State-Play -> State-Play)
(define (move-reflect s)
  (define-values (state-new reflected ball-new)
    (move-reflect-ball s (State-Play-ball s)
                       (* (State-dt s) BALL-SPEED)))
  (struct-copy
   State-Play state-new
   [ball ball-new]
   [ball-predicted-pos-ends (if reflected
                                (predict-ball-pos-ends-2 ball-new)
                                (State-Play-ball-predicted-pos-ends s))]))

(: FLONUM-SMOL : Flonum)
(define FLONUM-SMOL 0.00000001)

(: move-reflect-ball : State-Play Ball Flonum -> (Values State-Play Boolean Ball))
(define (move-reflect-ball s ball move-remaining)
  (define ball-dir (Ball-dir ball))
  (define ball-pos (Ball-pos ball))
  (define ball-pos-next (pos+ ball-pos
                              (dir-scale ball-dir move-remaining)))
  (define-values
    (axis-perpendicular axis-reflection dist-past)
    (get-bound-passed s ball-pos-next))
  (define is-out-of-movement (or (not axis-perpendicular)
                                 (not axis-reflection)
                                 (<= move-remaining 0.0)))
  (cond
    [is-out-of-movement
     (values s #f (struct-copy Ball ball [pos ball-pos-next]))]
    [else
     (define move-before-reflection (- move-remaining dist-past FLONUM-SMOL))
     (define-values (state-new _ ball-new)
       (move-reflect-ball
        (increment-score-maybe s axis-perpendicular)
        (struct-copy
         Ball ball
         [dir (dir-reflect ball-dir axis-reflection)]
         [pos
          (pos+ ball-pos
                (dir-scale (Ball-dir ball)
                           move-before-reflection))])
        dist-past))
     (values state-new #t ball-new)]))

(: increment-score-maybe : State-Play Dir -> State-Play)
(define (increment-score-maybe s axis-perpendicular)
  (cond [(eq? axis-perpendicular -x)
         (define player (State-Play-player s))
         (struct-copy State-Play s
                      [player (struct-copy
                               Player player
                               [score (get-new-player-score player 1)])])]
        [else s]))
