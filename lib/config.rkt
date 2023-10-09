#lang typed/racket/base

(require pict3d)

(provide (all-defined-out))

(: BALL-ACCELERATION-PADDLE : Flonum)
(define BALL-ACCELERATION-PADDLE 1.04)

(: BALL-ACCELERATION-WALL : Flonum)
(define BALL-ACCELERATION-WALL 1.01)

(: BALL-RADIUS : Flonum)
(define BALL-RADIUS 0.024)

(: BALL-SPEED : Flonum)
(define BALL-SPEED 0.001)

(: CAMERA-SPACE-DISTANCE : Flonum)
(define CAMERA-SPACE-DISTANCE 0.05)

(: BUMPER-SCALE : Dir)
(define BUMPER-SCALE (dir 0.015 0.11 0.03))

(: BUMPER-CONTACT-WIDTH : Flonum)
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))

(: BOUNDS-BUMPER-GAP : Flonum)
(define BOUNDS-BUMPER-GAP (* 10 (dir-dx BUMPER-SCALE)))

(: CAMERA-LOOK-AT : Pos)
(define CAMERA-LOOK-AT origin)

(: CONTACT-BUFFER : Flonum)
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))

(: FRAME-DELAY-MILLIS : Positive-Real)
(define FRAME-DELAY-MILLIS (max 1.0 (abs (/ 1000.0 59.9))))

(: OPPONENT-SPEED : Flonum)
(define OPPONENT-SPEED 0.001)

(: OPPONENT-X : Flonum)
(define OPPONENT-X -1.0)

(: OPPONENT-BOUNDS : Flonum)
(define OPPONENT-BOUNDS (- OPPONENT-X BOUNDS-BUMPER-GAP))

(: PLAYER-X : Flonum)
(define PLAYER-X 1.0)

(: PLAYER-BOUNDS : Flonum)
(define PLAYER-BOUNDS (+ PLAYER-X BOUNDS-BUMPER-GAP))

(: REDIRECT-FACTOR : Flonum)
(define REDIRECT-FACTOR 40.0)

(: SCREEN-WIDTH : Positive-Integer)
(define SCREEN-WIDTH 1200)

(: SCREEN-WIDTH-INEXACT : Flonum)
(define SCREEN-WIDTH-INEXACT (exact->inexact SCREEN-WIDTH))

(: SCREEN-HEIGHT : Positive-Integer)
(define SCREEN-HEIGHT 1080)

(: SCREEN-HEIGHT-INEXACT : Flonum)
(define SCREEN-HEIGHT-INEXACT (exact->inexact SCREEN-HEIGHT))

(: WALL-Y : Flonum)
(define WALL-Y 0.8)