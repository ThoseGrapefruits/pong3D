#lang typed/racket

(require
  math/flonum
  pict3d
  typed/racket/stream
  "../config.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt"
  "../util/player/index.rkt")

(provide on-frame-play-opponent)

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

(struct Pos-Dir
  ([pos : Pos]
   [dir : Dir]))

(: predict-ball-y : (->* (State-Play) ((Sequenceof Pos-Dir)) Flonum))
(define (predict-ball-y s [path (predict-ball-path s)])
  (define first (stream-first path))
  (define b-pos (Pos-Dir-pos first))
  (cond [(empty? path) 0.0] ; return to center
        [(fl= (pos-x b-pos) OPPONENT-X) (pos-y b-pos)]
        [else (predict-ball-y s (stream-rest path))]))

(: predict-ball-path : State-Play -> (Sequenceof Pos-Dir))
(define (predict-ball-path s)
  (define ball (State-Play-ball s))
  (predict-ball-path-internal s (Pos-Dir (Ball-pos ball) (Ball-dir ball))))

(: predict-ball-path-internal : State-Play Pos-Dir -> (Sequenceof Pos-Dir))
(define (predict-ball-path-internal s from)
  (define to (predict-next-ball-pos-dir from))
  (stream-cons to (predict-ball-path-internal s to)))

(: predict-next-ball-pos-dir : Pos-Dir -> Pos-Dir)
(define (predict-next-ball-pos-dir pd) pd) ; TODO
