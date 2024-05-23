#lang typed/racket/base

(require
  racket/set
  "../config.rkt"
  "../state/state.rkt"
  "../state/setters.rkt"
  "../util/pid.rkt")

(provide on-frame-play-player-position)

(: KEYBOARD-SPEED-FACTOR : Flonum)
(define KEYBOARD-SPEED-FACTOR 0.05)

(: on-frame-play-player-position : State-Play -> State-Play)
(define (on-frame-play-player-position s)
  (define player (State-Play-player s))
  (define player-pid (Player-y-pid player))
  (define player-y (Player-y player))
  (define player-y-desired (Player-y-desired player))
  (define pressed (State-pressed s))
  (define dt (State-dt s))
  (cond
    [(set-member? pressed "left")
      (State-set-player-position
       s
       (- player-y (* dt PLAYER-SPEED KEYBOARD-SPEED-FACTOR)))]
    [(set-member? pressed "right")
      (State-set-player-position
       s
       (+ player-y (* dt PLAYER-SPEED KEYBOARD-SPEED-FACTOR)))]
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