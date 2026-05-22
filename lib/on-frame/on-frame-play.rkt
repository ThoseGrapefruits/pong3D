#lang typed/racket/base

(require
  (only-in pict3d
           Pos
           pos-x
           pos-y)
  (only-in typed-compose compose-n)
  (only-in racket/math exact-floor)
  (only-in typed/racket/stream
           stream
           stream-filter
           stream-first)
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
            on-frame-play-tuner-tone
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
         (define state-fresh (state-reset-play s (unbox (State-n s)) (unbox (State-t s))))
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
            [score-multiplier (+ 1.0 (Player-score-multiplier player))]
            [score            (get-new-player-score player 10)])])]
        [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
         (define state-fresh (state-reset-play s (unbox (State-n s)) (unbox (State-t s))))
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

(define-values (base-tone update-base-tone) (tone-lerp 0.0 100.0))
(define-values (tuner-tone update-tuner-tone) (tone-lerp 0.0 100.0))

(: on-frame-play-tuner-tone : State-Play -> State-Play)
(define (on-frame-play-tuner-tone s)
  (define player (State-Play-player s))
  (define player-y-desired (Player-y-desired player))
  (define predicted-pos
    (stream-first (stream-filter (λ ([p : Pos]) (positive? (pos-x p)))
                                 (State-Play-ball-predicted-pos-ends s))))
  (cond
    [(and predicted-pos player-y-desired)
      (define base-tone (get-base-tone player-y-desired))
      (define diff (- (pos-y predicted-pos) player-y-desired))
      (update-base-tone base-tone)
      (update-tuner-tone (+ base-tone diff))
      s]
    [else s]))

(: get-base-tone : Flonum -> Flonum)
(define (get-base-tone player-y-desired)
  (exact->inexact (note-to-frequency (exact-floor (* 12 player-y-desired)))))