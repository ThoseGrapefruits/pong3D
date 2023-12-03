#lang typed/racket

(require
  math/flonum
  pict3d
  "../config.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt")

(provide on-frame-play-opponent)

(: on-frame-play-opponent : State-Play -> State-Play)
(define (on-frame-play-opponent s)
  (define ball (State-Play-ball s))
  (define ball-y (pos-y (Ball-pos ball)))
  (define opponent (State-Play-opponent s))
  (define aim-buffer (/ BUMPER-CONTACT-WIDTH 8))
  (define pos-predicted (State-Play-ball-predicted-pos s))
  (define pos-desired-y
    (cond [(null? pos-predicted) ball-y]
          [(positive? (dir-dx (Ball-dir ball))) 0.0]
          [(> (pos-x pos-predicted) OPPONENT-X-COLLISION) ball-y]
          [else (pos-y pos-predicted)]))
  (define pos-diff (- pos-desired-y (Opponent-y opponent)))
  (define y-desired (+ (Opponent-y opponent)
                       (cond
                         [(within?
                           pos-desired-y
                           (- (Opponent-y opponent) aim-buffer)
                           (+ (Opponent-y opponent) aim-buffer))
                          (* OPPONENT-SPEED
                             (State-dt s)
                             pos-diff
                             0.1)]
                         [else
                          (* OPPONENT-SPEED
                             (State-dt s)
                             (flsgn pos-diff))])))
  (struct-copy
   State-Play s
   [opponent
    (struct-copy
     Opponent opponent
     [y (clamp-bumper-y y-desired)])]))
