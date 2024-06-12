#lang typed/racket/base

(require
  racket/set
  pict3d
  typed-compose
  "../config.rkt"
  "../sound/sound.rkt"
  "../state/init.rkt"
  "../state/state.rkt"
  "../state/syntax.rkt"
  "../util/player.rkt"
  "./on-frame-play-ball.rkt"
  "./on-frame-play-opponent.rkt"
  "./on-frame-play-player-position.rkt")

(provide on-frame-play)

(: on-frame-play : State-Any -> State-Any)
(define (on-frame-play s)
  (cond [(State-Play? s)
         (on-frame-play-endgame
          ((compose-n ; bottom-to-top
            on-frame-play-lives
            on-frame-play-ball
            on-frame-play-opponent
            on-frame-play-player-position)
           s))]
        [else s]))

(: on-frame-play-endgame : State-Play -> (U State-Game-Over State-Play))
(define (on-frame-play-endgame s)
  (cond [(= 0 (Player-lives (State-Play-player s)))
         (rs-play SOUND-ENDGAME)
         (State-transition State-Game-Over s s)]
        [else s]))

(: on-frame-play-lives : State-Play -> State-Play)
(define (on-frame-play-lives s)
  (define ball (State-Play-ball s))
  (define player (State-Play-player s))
  (cond [(< (pos-x (Ball-pos ball)) OPPONENT-BOUNDS)
         (rs-play SOUND-SCORE-ON-OPPONENT)
         (define state-fresh (state-reset-play s (State-n s) (State-t s)))
         (struct-copy
          State-Play s
          [ball (State-Play-ball state-fresh)]
          [ball-predicted-pos-ends (State-Play-ball-predicted-pos-ends state-fresh)]
          [opponent (struct-copy
            Opponent (State-Play-opponent s)
            [y (Opponent-y (State-Play-opponent state-fresh))]
          )]
          [player
           (struct-copy
            Player player
            [score (get-new-player-score player 10)])])]
        [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
         (define state-fresh (state-reset-play s (State-n s) (State-t s)))
         (define player-lives-next (max 0 (sub1 (Player-lives player))))
         (if (positive? player-lives-next) (rs-play SOUND-SCORE-ON-PLAYER) null)
         ; Feels a bit excessive to generate an entire new fresh state, but we
         ; are using most of it here.
         (struct-copy
          State-Play s
          [ball (State-Play-ball state-fresh)]
          [ball-predicted-pos-ends (State-Play-ball-predicted-pos-ends state-fresh)]
          ; Get updated position & PID
          [opponent (State-Play-opponent state-fresh)]
          [player
           (struct-copy
            Player player
            [lives player-lives-next])])]
        [else s]))
