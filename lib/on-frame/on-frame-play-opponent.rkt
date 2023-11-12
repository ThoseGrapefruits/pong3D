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
  (define ball (State-Play-ball s))
  (define opponent (State-Play-opponent s))
  (define aim-buffer (/ BUMPER-CONTACT-WIDTH 8))
  (define pos-desired (pos-y (Ball-pos ball)))
  (define pos-diff (- pos-desired (Opponent-y opponent)))
  (define y-desired (+ (Opponent-y opponent)
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
                             (flsgn pos-diff))])))
  (struct-copy
   State-Play s
   [opponent
    (struct-copy
     Opponent opponent
     [y (clamp-bumper-y y-desired)])]))

(struct Pos-Dir
  ([pos : Pos]
   [dir : Dir]))

(: predict-ball-y : (->* (State-Play) ((Sequenceof Pos-Dir)) Flonum))
(define (predict-ball-y s [path (predict-ball-path s)])
  (define current-pos-dir (stream-first path))
  (match-define (Pos-Dir b-pos b-dir) current-pos-dir)
  (cond [(empty? path) 0.0] ; return to center
        [(fl= (pos-x b-pos) OPPONENT-X) (pos-y b-pos)]
        [else (predict-ball-y s (stream-rest path))]))

(: predict-ball-path : State-Play -> (Sequenceof Pos-Dir))
(define (predict-ball-path s)
  (define ball (State-Play-ball s))
  (predict-ball-path-internal (Pos-Dir (Ball-pos ball) (Ball-dir ball))))

(: predict-ball-path-internal : Pos-Dir -> (Sequenceof Pos-Dir))
(define (predict-ball-path-internal from)
  (define to (predict-next-ball-pos-dir from))
  (stream-cons to (predict-ball-path-internal to)))

(: predict-next-ball-pos-dir : Pos-Dir -> Pos-Dir)
(define (predict-next-ball-pos-dir pd)
  (match-define (Pos-Dir p d) pd)
  (match-define-values (yaw pitch) (dir->angles d))
  (define wall-y (cond [(positive? (dir-dy d)) BALL-MAX-Y    ]
                       [else                   (- BALL-MAX-Y)]))
  (define end-x (cond [(positive? (dir-dx d)) PLAYER-X   ]
                      [else                  OPPONENT-X ]))
  (define tan-pitch (tan pitch))

  (define end-dx (- end-x (pos-x p)))
  (define end-dy (* end-dx tan-pitch))
  (define wall-dy (- wall-y (pos-y p)))
  (define wall-dx (/ wall-dy tan-pitch))
  (define dx (min end-dx wall-dx))
  (define dy (min end-dy wall-dy))

  (Pos-Dir (pos+ p (dir dx dy 0.0))
           (dir-reflect d +y)))
