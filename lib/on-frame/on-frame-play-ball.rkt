#lang typed/racket

(require
  pict3d
  "../config.rkt"
  "../state/accessors.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt")

(provide on-frame-play-ball)

(: on-frame-play-ball : State-Play -> State-Play)
(define (on-frame-play-ball s)
  ((compose1
    on-frame-play-ball-position
    on-frame-play-ball-direction) s))

(: on-frame-play-ball-collision-bumper : State-Play -> State-Play)
(define (on-frame-play-ball-collision-bumper s)
  (let ([player (State-Play-player s)]
        [opponent (State-Play-opponent s)]
        [ball (State-Play-ball s)])
    (cond
      ; opponent collision
      [(and (negative? (dir-dx (Ball-dir ball)))
            (within? (+ (pos-x (Ball-pos ball)) CONTACT-BUFFER)
                     (+ OPPONENT-X CONTACT-BUFFER)
                     (+ OPPONENT-X (* CONTACT-BUFFER 2)))
            (within? (State-get-contact-offset-y s (Opponent-y opponent))
                     (- (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir
                (dir-scale
                 (dir-reflect (Ball-dir ball)
                              (angles->dir
                               (* (- 180 REDIRECT-FACTOR)
                                  (State-get-contact-offset-y s (Opponent-y opponent)))
                               0))
                 BALL-ACCELERATION-PADDLE)])])]
      ; player collision
      [(and (positive? (dir-dx (Ball-dir ball)))
            (within? (+ (pos-x (Ball-pos ball)) CONTACT-BUFFER)
                     (- PLAYER-X CONTACT-BUFFER)
                     PLAYER-X)
            (within? (State-get-contact-offset-y s (Player-y player))
                     (- (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State-Play s
        [ball
         (struct-copy
          Ball ball
          [dir (dir-scale (dir-reflect
                           (Ball-dir ball)
                           (angles->dir
                            (* (- REDIRECT-FACTOR)
                               (State-get-contact-offset-y s (Player-y player)))
                            0))
                          BALL-ACCELERATION-PADDLE)])]
        [player
         (struct-copy
          Player player
          [score (get-new-player-score player 1)])])]
      [else s])))

(: on-frame-play-ball-collision-wall : State-Play -> State-Play)
(define (on-frame-play-ball-collision-wall s)
  (let ([plr (State-Play-player s)]
        [ball (State-Play-ball s)])
    (cond
      ; left wall collision
      [(and (negative? (dir-dy (Ball-dir ball)))
            (< (pos-y (Ball-pos ball)) (- (- WALL-Y BALL-RADIUS))))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir (dir-scale
                     (dir-reflect (Ball-dir ball) +y)
                     BALL-ACCELERATION-WALL)])])]
      ; right wall collision
      [(and (positive? (dir-dy (Ball-dir ball)))
            (> (pos-y (Ball-pos ball)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir (dir-scale
                     (dir-reflect (Ball-dir ball) +y)
                     BALL-ACCELERATION-WALL)])])]
      [else s])))

(: on-frame-play-ball-direction : State-Play -> State-Play)
(define (on-frame-play-ball-direction s)
  ((compose1
    on-frame-play-ball-collision-bumper
    on-frame-play-ball-collision-wall) s))

(: on-frame-play-ball-position : State-Play -> State-Play)
(define (on-frame-play-ball-position s)
  (let ([ball (State-Play-ball s)])
    (struct-copy
     State-Play s
     [ball (struct-copy
            Ball ball
            [pos
             (pos+ (Ball-pos ball)
                   (dir-scale (Ball-dir ball)
                              (* (State-dt s) BALL-SPEED)))])])))
