#lang typed/racket

(require
  pict3d
  typed-compose
  "../config.rkt"
  "../state/init.rkt"
  "../state/setters.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt"
  "../util/player/index.rkt"
  "./on-frame-play-ball.rkt"
  "./on-frame-play-opponent.rkt")

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

(: on-frame-play-endgame : State-Play -> (U State-Game-Over State-Play))
(define (on-frame-play-endgame s)
  (cond [(= 0 (Player-lives (State-Play-player s)))
         (State-transition State-Game-Over s s)]
        [else s]))

(: on-frame-play-player-position : State-Play -> State-Play)
(define (on-frame-play-player-position s)
  (define player (State-Play-player s))
  (define pressed (State-pressed s))
  (cond
    [(set-member? pressed "left")
     (State-set-player-position
      s
      (+ (Player-y player) (* (State-dt s) -1/512)))]
    [(set-member? pressed "right")
     (State-set-player-position
      s
      (+ (Player-y player) (* (State-dt s)  1/512)))]
    [else s]))

(: on-frame-play-lives : State-Play -> State-Play)
(define (on-frame-play-lives s)
  (define ball (State-Play-ball s))
  (define player (State-Play-player s))
  (cond [(< (pos-x (Ball-pos ball)) OPPONENT-BOUNDS)
         (struct-copy
          State-Play s
          [ball (state-start-game-play-ball)]
          [ball-predicted-pos null]
          [player
           (struct-copy
            Player player
            [score (get-new-player-score player 10)])])]
        [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
         (struct-copy
          State-Play s
          [ball (state-start-game-play-ball)]
          [ball-predicted-pos null]
          [player
           (struct-copy
            Player player
            [lives (max 0 (sub1 (Player-lives player)))])])]
        [else s]))
