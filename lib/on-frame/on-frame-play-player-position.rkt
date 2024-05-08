#lang typed/racket/base

(require
  racket/set
  "../config.rkt"
  "../state/state.rkt"
  "../state/setters.rkt"
  "../util/pid.rkt")

(provide on-frame-play-player-position)

(: on-frame-play-player-position : State-Play -> State-Play)
(define (on-frame-play-player-position s)
  (define player (State-Play-player s))
  (define player-pid (Player-y-pid player))
  (define player-y (Player-y player))
  (define player-y-desired (Player-y-desired player))
  (define pressed (State-pressed s))
  (define dt (State-dt s))
  (define speed (* dt PLAYER-SPEED))
  (cond
    [(set-member? pressed "left")
      (State-set-player-position
       s
       (- player-y (/ speed 10.0)))]
    [(set-member? pressed "right")
      (State-set-player-position
       s
       (+ player-y (/ speed 10.0)))]
    [player-y-desired
     (define dt (State-dt s))
     (define diffff-y (- player-y-desired player-y))
     (define max-err (* dt PLAYER-SPEED))
     (define diffff-y-clamped (max (- max-err)
                                   (min max-err diffff-y)))
     (State-set-player-position
      s
      (+ (pid-step! player-pid diffff-y-clamped dt)
         player-y)
      player-y-desired)]
    [else s]))