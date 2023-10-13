#lang typed/racket

(require
  math/flonum 
  pict3d
  typed-compose
  typed/racket/gui
  "../config.rkt"
  "../state/accessors.rkt"
  "../state/init.rkt"
  "../state/setters.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt")

(provide on-frame-play)

(: on-frame-play : State -> State)
(define (on-frame-play s)
  (cond [(State-Play? s)
          (on-frame-play-endgame
           ((compose-n
             on-frame-play-ball
             on-frame-play-opponent
             on-frame-play-player-position
             on-frame-play-lives)
            s))]
        [else s]))

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

(: on-frame-play-endgame : State-Play -> (U State-Game-Over State-Play))
(define (on-frame-play-endgame s)
  (cond [(= 0 (Player-lives (State-Play-player s)))
         (State-transition State-Game-Over s s)]
        [else s]))

(: on-frame-play-opponent : State-Play -> State-Play)
(define (on-frame-play-opponent s)
  (let ([ball (State-Play-ball s)]
        [opponent (State-Play-opponent s)])
    (let ([aim-buffer (/ BUMPER-CONTACT-WIDTH 8)]
          [pos-desired (pos-y (Ball-pos ball))])
      (let ([pos-diff (- pos-desired (Opponent-y opponent))])
        (struct-copy
         State-Play s
         [opponent (struct-copy
                    Opponent opponent
                    [y (clamp-bumper-y (+ (Opponent-y opponent)
                          (cond
                            [(within?
                              (pos-y (Ball-pos ball))
                              (- (Opponent-y opponent) aim-buffer)
                              (+ (Opponent-y opponent) aim-buffer))
                             (* OPPONENT-SPEED
                                (State-dt s)
                                pos-diff)]
                            [else
                             (* OPPONENT-SPEED
                                (State-dt s)
                                (flsgn pos-diff))])))])])))))

(: on-frame-play-player-position : State-Play -> State-Play)
(define (on-frame-play-player-position s)
  (let ([player (State-Play-player s)]
        [pressed (State-pressed s)])
    (cond
      [(set-member? pressed "left")
       (State-set-player-position
        s
        (+ (Player-y player) (* (State-dt s) -1/512)))]
      [(set-member? pressed "right")
       (State-set-player-position
        s
        (+ (Player-y player) (* (State-dt s)  1/512)))]
      [else s])))

(: on-frame-play-lives : State-Play -> State-Play)
(define (on-frame-play-lives s)
  (let ([ball (State-Play-ball s)]
        [player (State-Play-player s)])
    (cond [(< (pos-x (Ball-pos ball)) OPPONENT-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [player
             (struct-copy
              Player player
              [score (get-new-player-score player 10)])])]
          [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [player
             (struct-copy
              Player player
              [lives (max 0 (sub1 (Player-lives player)))])])]
          [else s])))
