#lang typed/racket

(require
  math/flonum
  pict3d
  "../config.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt"
  "../util/pid.rkt")

(provide on-frame-play-opponent)

(define pid-position (pid 0.1 0.01 0.0001))

(: on-frame-play-opponent : State-Play -> State-Play)
(define (on-frame-play-opponent s)
  (define ball (State-Play-ball s))
  (define ball-y (pos-y (Ball-pos ball)))
  (define opponent (State-Play-opponent s))
  (define pos-predicted-maybe
    (findf (λ ([p : Pos]) (negative? (pos-x p)))
           (State-Play-ball-predicted-pos-ends s)))
  (define pos-predicted (if (false? pos-predicted-maybe) null pos-predicted-maybe));
  (define pos-desired-y
    (cond [(null? pos-predicted) ball-y]
          [(> (pos-x pos-predicted) OPPONENT-X-COLLISION) ball-y]
          [else (pos-y pos-predicted)]))
  ; clamping here helps the pid controller's integral not get mad when it can't
  ; reach the edge of the stage
  (define pos-diff (- (clamp-bumper-y pos-desired-y) (Opponent-y opponent)))
  (define y-desired (pid-position pos-diff (State-dt s)))
  (struct-copy
   State-Play s
   [opponent
    (struct-copy
     Opponent opponent
     [y (clamp-bumper-y y-desired)])]))
