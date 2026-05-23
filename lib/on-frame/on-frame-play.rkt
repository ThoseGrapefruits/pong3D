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
  "../sound/rsound.rkt"
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

(define-values (base-tone update-base-tone) (tone-lerp 20.0 80.0))
(define-values (tuner-tone update-tuner-tone) (tone-lerp 20.0 80.0))

(: on-frame-play-tuner-tone : State-Play -> State-Play)
(define (on-frame-play-tuner-tone s)
  (define ttp-box (State-Play-tuner-tone-playing s))
  (when (not (unbox ttp-box))
    (set-box! ttp-box #t)
    (rs:signal-play base-tone)
    (rs:signal-play tuner-tone))
  (when (= 0 (modulo (unbox (State-n s)) 5))
    (define player (State-Play-player s))
    (define player-y (or (Player-y-desired player) (Player-y player)))
    (define predicted-pos
      (stream-first (stream-filter (λ ([p : Pos]) (positive? (pos-x p)))
                                   (State-Play-ball-predicted-pos-ends s))))
    (define predicted-y (and predicted-pos (pos-y predicted-pos)))
    (when player-y
      (define base-tone (get-base-tone (or predicted-y player-y)))
      (define diff (- predicted-y player-y))
      (update-base-tone base-tone)
      (update-tuner-tone (+ base-tone (* diff 15.0)))))
    s)

(: get-base-tone : Flonum -> Flonum)
(define (get-base-tone y)
  (define n (exact-floor (* 12 y)))
  (define sgn (- 1 (* 2 (modulo n 2))))
  (exact->inexact (note-to-frequency (* sgn (modulo n 5) (modulo n 3)))))
