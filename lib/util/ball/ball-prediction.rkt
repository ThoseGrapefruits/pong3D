#lang typed/racket

; This module is used to predict the end position of the ball by the time it
; reaches the end of the arena it's currently moving towards. It's the
; foundation of a smart opponent that can pre-position themselves to receive the
; ball without having to rely on fast movement speeds

;          ⊖ ─ y ─ ⊕
;             ╔═════ end-dy ══════╗
; ┏ ╔         ━━━┄┄┄┄┄┄┄┄┄┄┄┓┄┄┄┄┄┄╱
; ┃ ║         ┆             ┃    ╱
; ┃ ║         ┆             ┃  ╱
; ┃ ║         ╔═ wall-dy ═══╗╱
; ┃ ║       ╔ ┆‾‾‾‾‾‾‾‾‾‾‾‾╱┃
; ┃ end-dx  ║ ┆          ╱  ┃         ⊖
; ┃ ║       ║ ┆        ╱    ┃         │
; ┃ ║ wall-dx ┆      ╱      ┃         x
; ┃ ║       ║ ┆    ╱        ┃         │
; ┃ ║       ║ ┆  ╱          ┃         ⊕
; ┃ ╚       ╚╭╶╮⬈           ┃
; ┃          ╰╴╯            ┃
; ┃                         ┃
; ┃                         ┃
; ┗           ━━━           ┛
;
;          ⊖ ─ y ─ ⊕


(require
  math/flonum
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
        [(and (<= (pos-x b-pos) OPPONENT-X)
              (negative? (dir-dx b-dir)))
         b-pos] ; hit opponent bound
        [(and (>= (pos-x b-pos) PLAYER-X)
              (positive? (dir-dx b-dir)))
         b-pos] ; hit player bound
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
  (define dx-source (dir-dx d))
  (define dy-source (dir-dy d))
  (define wall-y (cond [(positive? dy-source)    BALL-MAX-Y ]
                       [else           (- BALL-MAX-Y)]))
  (define end-x (cond [(positive? dx-source) PLAYER-X   ]
                      [else           OPPONENT-X ]))
  (define tan-theta (/ dy-source dx-source))

  (define end-dx (- end-x (pos-x p)))
  (define end-dy (* end-dx tan-theta))
  (define wall-dy (- wall-y (pos-y p)))
  (define wall-dx (/ wall-dy tan-theta))
  (define dx (if (positive? end-dx) (min end-dx wall-dx) (max end-dx wall-dx)))
  (define dy (if (positive? end-dy) (min end-dy wall-dy) (max end-dy wall-dy)))

  (Pos-Dir (pos+ p (dir dx dy 0.0))
           (dir-reflect d +y)))