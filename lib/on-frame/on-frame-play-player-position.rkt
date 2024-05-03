#lang typed/racket

(require
  "../config.rkt"
  "../state/state.rkt"
  "../state/setters.rkt"
  "../util/pid.rkt")

(provide on-frame-play-player-position)

(define pid-position (make-pid #:tuning-p 0.1
                               #:tuning-i 0.01
                               #:tuning-d 0.0001))

(: on-frame-play-player-position : State-Play -> State-Play)
(define (on-frame-play-player-position s)
  (define player (State-Play-player s))
  (define pressed (State-pressed s))
  (define dt (State-dt s))
  (define speed (* dt PLAYER-SPEED))
  (cond
    [(set-member? pressed "left")
      (pid-reset! pid-position)
      (State-set-player-position
       s
       (- (Player-y player) (/ speed 2.0)))]
    [(set-member? pressed "right")
      (pid-reset! pid-position)
      (State-set-player-position
       s
       (+ (Player-y player) (/ speed 2.0)))]
    [else
     (define dt (State-dt s))
     (define diffff-y (- (Player-y-desired player)
                         (Player-y player)))
     (define max-err (* dt PLAYER-SPEED))
     (define diffff-y-clamped (max (- max-err)
                                   (min max-err diffff-y)))
     (State-set-player-position
      s
      (pid-step! pid-position diffff-y-clamped dt)
      (Player-y-desired player))]))