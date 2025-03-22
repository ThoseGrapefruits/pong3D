#lang typed/racket/base

; This module is used to predict the end position of the ball by the time it
; reaches the end of the arena it's currently moving towards. It's the
; foundation of a smart opponent that can pre-position themselves to receive the
; ball without having to rely on fast movement speeds.

(require
  pict3d
  racket/list
  (only-in racket/match match-define)
  typed/racket/stream
  "../config.rkt"
  "../state/state.rkt")

(struct Pos-Dir
  ([pos : Pos]
   [dir : Dir])
  #:transparent)

(provide
  predict-ball-pos
  predict-ball-pos-ends
  predict-ball-pos-ends-2)

(: predict-ball-pos : Ball -> Pos)
(define (predict-ball-pos ball)
  (stream-first (predict-ball-pos-ends ball)))

(: predict-ball-pos-ends-2 : Ball -> (Listof Pos))
(define (predict-ball-pos-ends-2 ball)
  (stream->list (stream-take (predict-ball-pos-ends ball) 2)))

(: predict-ball-pos-ends : (->* (Ball) ((Sequenceof Pos-Dir)) (Sequenceof Pos)))
(define (predict-ball-pos-ends ball [path (predict-ball-path ball)])
  (define current-pos-dir (stream-first path))
  (match-define (Pos-Dir b-pos b-dir) current-pos-dir)
  (cond [(empty? path) 0.0] ; return to center
        [(or (and (<= (pos-x b-pos) OPPONENT-X-COLLISION) ; hit opponent bound
                  (positive? (dir-dx b-dir)))
             (and (>= (pos-x b-pos) PLAYER-X-COLLISION) ; hit player bound
                  (negative? (dir-dx b-dir))))
         (stream-cons b-pos (predict-ball-pos-ends ball (stream-rest path)))]
        [else
         (predict-ball-pos-ends ball (stream-rest path))]))

(: predict-ball-path : Ball -> (Sequenceof Pos-Dir))
(define (predict-ball-path ball)
  (predict-ball-path-internal (Pos-Dir (Ball-pos ball) (Ball-dir ball))))

(: predict-ball-path-internal : Pos-Dir -> (Sequenceof Pos-Dir))
(define (predict-ball-path-internal from)
  (define to (predict-next-ball-pos-dir from))
  (stream-cons to (predict-ball-path-internal to)))

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
; ┃ ║       ║ ┆──╮ ╱        ┃         │
; ┃ ║       ║ ┆θ ╱          ┃         ⊕
; ┃ ╚       ╚╭╶╮⬈           ┃
; ┃          ╰╴╯            ┃
; ┃                         ┃
; ┃                         ┃
; ┗           ━━━           ┛
;
;          ⊖ ─ y ─ ⊕

(: predict-next-ball-pos-dir : Pos-Dir -> Pos-Dir)
(define (predict-next-ball-pos-dir pd)
  (match-define (Pos-Dir p d) pd)
  (define dx-source (dir-dx d))
  (define dy-source (dir-dy d))
  (define wall-y (if (positive? dy-source)
                     BALL-MAX-Y
                     (- BALL-MAX-Y)))
  (define end-x (if (positive? dx-source)
                    PLAYER-X-COLLISION
                    OPPONENT-X-COLLISION))
  (define tan-θ (/ dy-source dx-source))

  (define end-dx (- end-x (pos-x p)))
  (define end-dy (* end-dx tan-θ))
  (define wall-dy (- wall-y (pos-y p)))
  (define wall-dx (/ wall-dy tan-θ))
  (define dx (if (positive? end-dx)
                 (min end-dx wall-dx)
                 (max end-dx wall-dx)))
  (define dy (if (positive? end-dy)
                 (min end-dy wall-dy)
                 (max end-dy wall-dy)))
  (define pos-new (pos+ p (dir dx dy 0.0)))
  (define dir-new (if (or (and (<= (pos-x pos-new) OPPONENT-X-COLLISION) ; hit opponent bound
                               (negative? dx-source))
                          (and (>= (pos-x pos-new) PLAYER-X-COLLISION) ; hit player bound
                               (positive? dx-source)))
                      (dir-reflect d +x)
                      (dir-reflect d +y)))

  (Pos-Dir pos-new dir-new))