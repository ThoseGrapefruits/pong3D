#lang typed/racket

(require
  pict3d
  typed/racket/stream
  "../../config.rkt"
  "../../state/state.rkt")

(struct Pos-Dir
  ([pos : Pos]
   [dir : Dir]))

(provide predict-ball-pos)

(: predict-ball-pos : (->* (Ball) ((Sequenceof Pos-Dir)) Pos))
(define (predict-ball-pos ball [path (predict-ball-path ball)])
  (define current-pos-dir (stream-first path))
  (match-define (Pos-Dir b-pos b-dir) current-pos-dir)
  (cond [(empty? path) 0.0] ; return to center
        [(<= (pos-x b-pos) OPPONENT-X) b-pos] ; hit opponent bound
        [(>= (pos-x b-pos) PLAYER-X)   b-pos] ; hit player bound
        [else (predict-ball-pos ball (stream-rest path))]))

(: predict-ball-path : Ball -> (Sequenceof Pos-Dir))
(define (predict-ball-path ball)
  (predict-ball-path-internal (Pos-Dir (Ball-pos ball) (Ball-dir ball))))

(: predict-ball-path-internal : Pos-Dir -> (Sequenceof Pos-Dir))
(define (predict-ball-path-internal from)
  (define to (predict-next-ball-pos-dir from))
  (stream-cons to (predict-ball-path-internal to)))

(: predict-next-ball-pos-dir : Pos-Dir -> Pos-Dir)
(define (predict-next-ball-pos-dir pd)
  (match-define (Pos-Dir p d) pd)
  (writeln (format "pos ~a dir ~a" p d))
  ; TODO 180 yaw w/ reverse direction
  (match-define-values (yaw pitch) (dir->angles d))
  (define wall-y (cond [(positive? (dir-dy d)) BALL-MAX-Y    ]
                       [else                   (- BALL-MAX-Y)]))
  (define end-x (cond [(positive? (dir-dx d)) PLAYER-X   ]
                      [else                  OPPONENT-X ]))
  (define tan-yaw (tan yaw))

  (writeln (format "yaw ~a pitch ~a" yaw pitch))

  (define end-dx (- end-x (pos-x p)))
  (define end-dy (* end-dx tan-yaw))
  (define wall-dy (- wall-y (pos-y p)))
  (define wall-dx (/ wall-dy tan-yaw))
  (define dx (if (positive? end-dx) (min end-dx wall-dx) (max end-dx wall-dx)))
  (define dy (if (positive? end-dy) (min end-dy wall-dy) (max end-dy wall-dy)))

  (Pos-Dir (pos+ p (dir dx dy 0.0))
           (dir-reflect d +y)))